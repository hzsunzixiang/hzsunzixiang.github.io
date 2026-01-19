---
title: RabbitMQ 依赖分析：Systemd 完整指南
date: 2026-01-19
categories:
  - RabbitMQ
  - RabbitMQ Deps
tags:
  - RabbitMQ
  - Erlang
  - Systemd
  - Linux
  - 依赖分析
---

# Systemd 完整指南：从原理到 RabbitMQ 实践

## 1. Systemd 概述

### 1.1 什么是 Systemd？

Systemd 是 Linux 系统的系统和服务管理器，负责：
- 启动和管理系统服务
- 管理服务依赖关系
- 并行启动服务以加快启动速度
- 监控服务状态并自动重启

### 1.2 核心组件

```
┌─────────────────────────────────────────────────────────────────┐
│                         systemd                                  │
│                    (服务管理器 PID 1)                            │
├─────────────────────────────────────────────────────────────────┤
│  systemctl      │  journalctl    │  systemd-notify              │
│  (管理命令)      │  (日志查看)     │  (状态通知)                  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Service Units (.service)                      │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────────────┐ │
│  │ nginx    │  │ sshd     │  │ mysql    │  │ rabbitmq-server  │ │
│  └──────────┘  └──────────┘  └──────────┘  └──────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

---

## 2. 核心术语

| 术语 | 英文 | 说明 |
|------|------|------|
| **应用程序** | Service Script (App) | 实际运行的程序，如 `helloworld.sh` |
| **服务单元** | Service Unit | 描述服务的配置文件，如 `helloworld.service` |
| **服务管理器** | systemd | 系统和服务管理守护进程 (PID 1) |
| **管理命令** | systemctl | 与 systemd 交互的命令行工具 |
| **通知机制** | sd_notify / systemd-notify | 服务向 systemd 报告状态的机制 |

---

## 3. 服务通知机制

### 3.1 为什么需要通知机制？

应用程序启动需要一定时间才能完成初始化。systemd 需要知道服务何时真正就绪：

```
┌─────────────┐    启动命令     ┌─────────────────┐
│   systemd   │ ─────────────► │   应用程序       │
│             │                │                  │
│  等待通知... │ ◄───READY=1─── │ 初始化完成后通知  │
│             │                │                  │
│  服务已就绪  │                │  继续运行...     │
└─────────────┘                └─────────────────┘
```

### 3.2 服务类型 (Type)

| Type | 描述 | 就绪判断 |
|------|------|---------|
| `simple` | 默认类型 | `ExecStart` 进程启动即就绪 |
| `forking` | 传统守护进程 | 主进程 fork 后退出即就绪 |
| **`notify`** | 通知类型 | 收到 `READY=1` 通知即就绪 |
| `oneshot` | 一次性任务 | 进程退出即完成 |

### 3.3 通知协议

通过 Unix Socket (`NOTIFY_SOCKET`) 发送状态：

```bash
# 环境变量
NOTIFY_SOCKET=/run/systemd/notify

# 常用通知消息
READY=1                    # 服务就绪
STATUS=Processing data     # 状态描述（显示在 systemctl status）
MAINPID=12345             # 主进程 PID
STOPPING=1                # 正在停止
RELOADING=1               # 正在重载配置
```

### 3.4 systemd-notify 命令

```bash
# 通知就绪
systemd-notify --ready

# 更新状态
systemd-notify --status="Processing request..."

# 组合使用
systemd-notify --ready --status="Waiting for connections"
```

**手册摘要**：
```
NAME
    systemd-notify - 通知服务管理器启动完成及其他状态变化

SYNOPSIS
    systemd-notify [OPTIONS...] [VARIABLE=VALUE...]

OPTIONS
    --ready       通知服务启动完成 (等价于 READY=1)
    --status=     发送人类可读的状态字符串
    --pid=        通知主进程 PID
    --stopping    通知正在停止
    --reloading   通知正在重载
```

---

## 4. 四种语言实现示例

### 4.1 Shell 实现

**服务脚本** `helloworld.sh`:
```bash
#!/bin/bash

# 通知 systemd 服务已就绪
systemd-notify --ready --status="Waiting for data …....."
sleep 10

while : ; do
    echo "...NOTIFY_SOCKET:[ $NOTIFY_SOCKET ]..." > /tmp/notify.txt
    systemd-notify --status="do something hello....."   
    sleep 5 
    systemd-notify --status="do something world....."   
    sleep 5 
done
```

**服务单元** `helloworld.service`:
```ini
[Unit]
Description=HelloWorld Service

[Service]
Type=notify
ExecStart=/usr/local/bin/helloworld.sh

[Install]
WantedBy=multi-user.target
```

**运行效果**:
```bash
$ systemctl status helloworld.service
● helloworld.service - HelloWorld Service
   Loaded: loaded (/etc/systemd/system/helloworld.service; disabled)
   Active: active (running) since Wed 2023-10-04 06:22:52 EDT
 Main PID: 7039 (helloworld.sh)
   Status: "do something hello....."
    Tasks: 1
   CGroup: /system.slice/helloworld.service
           └─7039 /bin/bash /usr/local/bin/helloworld.sh
```

---

### 4.2 C 语言实现

**服务程序** `helloworld_c.c`:
```c
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <stdlib.h>
// yum install systemd-devel
#include <systemd/sd-daemon.h>

int main(int argc, char *argv[]) {
    printf("Starting up ...");
    sleep(5);
    printf("Startup complete before notify");
    
    // 通知 systemd 服务就绪
    sd_notify(0, "READY=1");
    
    sleep(5);
    sd_notify(0, "STATUS=status message");
    sleep(5);

    for(;;) {
        printf("Hello from the Demo Service");
        sd_notify(0, "STATUS=Processing  data");
        sleep(5);
        sd_notify(0, "STATUS=Waiting for data");
        sleep(5);
    }
    return 0;
}
```

**编译**:
```bash
# 安装开发库
sudo yum install systemd-devel

# 编译
gcc -o helloworld_c helloworld_c.c -lsystemd
```

**服务单元** `helloworld_c.service`:
```ini
[Unit]
Description=HelloWorld Service

[Service]
Type=notify
ExecStart=/usr/local/bin/helloworld_c

[Install]
WantedBy=multi-user.target
```

**核心 API**:
```c
// sd_notify 函数原型
int sd_notify(int unset_environment, const char *state);

// 常用调用
sd_notify(0, "READY=1");                  // 就绪通知
sd_notify(0, "STATUS=Processing...");     // 状态更新
sd_notify(0, "MAINPID=%d", getpid());     // PID 通知
sd_notify(0, "STOPPING=1");               // 停止通知
```

---

### 4.3 Python 实现

**服务程序** `helloworld.py`:
```python
if __name__ == '__main__':
    import time
    import systemd.daemon

    print('Starting up ...')
    time.sleep(5)
    print('Startup complete before notify')
    
    # 通知 systemd 服务就绪
    systemd.daemon.notify('READY=1')
    print('Startup complete after notify')

    while True:
        print('Hello from the Python Demo Service')
        systemd.daemon.notify("STATUS=Processing  data")
        time.sleep(5)
        systemd.daemon.notify("STATUS=Waiting for data")
        time.sleep(5)
```

**依赖安装**:
```bash
pip install systemd-python
# 或
yum install python-systemd
```

**服务单元** `helloworld.service`:
```ini
[Unit]
Description=HelloWorld Service

[Service]
Type=notify
ExecStart=/usr/bin/python /usr/local/bin/helloworld.py

[Install]
WantedBy=multi-user.target
```

---

### 4.4 Erlang 实现

Erlang 使用 [systemd](https://github.com/hauleth/systemd) 库与 systemd 集成。

**应用入口** `erlang_systemd_app.erl`:
```erlang
-module(erlang_systemd_app).

-export([boot/0]).
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    boot().

boot() ->
    erlang_systemd_sup:start_link().

stop(_State) ->
    ok.
```

**监督树** `erlang_systemd_sup.erl`:
```erlang
-module(erlang_systemd_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 3600},
    ChildSpecs = [child_static(frequency)],
    {ok, {SupFlags, ChildSpecs}}.

child_static(Module) ->
    {Module, {Module, start_link, []},
     permanent, 5000, worker, [Module]}.
```

**核心工作进程** `frequency.erl`:
```erlang
-module(frequency).
-behaviour(gen_server).
-compile(export_all).

start_link() ->
    io:format('Starting up ...\n'),
    io:format('Startup complete before notify\n'),
    
    %% 启动 systemd 应用
    {ok, _} = application:ensure_all_started(systemd),
    timer:sleep(5000),
    
    %% 通知就绪
    systemd:notify([ready, {status, "booting"}]),
    io:format('after ready: Startup complete after notify\n'),
    
    timer:sleep(5000),
    systemd:notify([{status, "Processing  data"}]),
    io:format('after:Processing  data: Startup complete after notify\n'),
    
    timer:sleep(5000),
    systemd:notify([{status, "After Processing  data"}]),
    io:format('after:After Processing  data : Startup complete after notify\n'),
    
    timer:sleep(5000),
    systemd:notify([ready, {status, "booted"}]),
    io:format('after:ready booted \n'),
    
    gen_server:start_link({local, frequency}, frequency, [], []).

%% ... gen_server 回调函数 ...
```

**服务单元** `helloworld.service`:
```ini
[Unit]
Description=HelloWorld Service

[Service]
Type=notify
NotifyAccess=all
ExecStart=/usr/local/bin/helloworld.sh

[Install]
WantedBy=multi-user.target
```

**关键配置**: `NotifyAccess=all` - 允许服务的任何子进程发送通知（Erlang VM 的通知来自子进程）

**Erlang systemd API**:
```erlang
%% 通知就绪
systemd:notify([ready]).

%% 带状态的就绪通知
systemd:notify([ready, {status, "Initialized"}]).

%% 状态更新
systemd:notify([{status, "Processing data..."}]).

%% 停止通知
systemd:notify([stopping]).
```

---

## 5. RabbitMQ 与 Systemd 集成

### 5.1 RabbitMQ 服务单元

```ini
# /usr/lib/systemd/system/rabbitmq-server.service
[Unit]
Description=RabbitMQ broker
After=network.target epmd@0.0.0.0.socket
Wants=network.target epmd@0.0.0.0.socket

[Service]
Type=notify
NotifyAccess=all
User=rabbitmq
Group=rabbitmq
UMask=0027
NotifyAccess=all
TimeoutStartSec=3600
LimitNOFILE=32768
Restart=on-failure
RestartSec=10
WorkingDirectory=/var/lib/rabbitmq
ExecStart=/usr/lib/rabbitmq/bin/rabbitmq-server
ExecStop=/usr/lib/rabbitmq/bin/rabbitmqctl shutdown

[Install]
WantedBy=multi-user.target
```

### 5.2 启动环境变量

RabbitMQ 在 systemd 环境下设置：

```bash
NOTIFY_SOCKET=/run/systemd/notify
RUNNING_UNDER_SYSTEMD=true
```

### 5.3 启动流程

```
┌─────────────────────────────────────────────────────────────────┐
│ systemctl start rabbitmq-server                                 │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 1. systemd 执行 ExecStart=/usr/lib/rabbitmq/bin/rabbitmq-server │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 2. 启动 Erlang VM (beam.smp)                                    │
│    NOTIFY_SOCKET 环境变量传递给 VM                               │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 3. rabbit:boot/0 - RabbitMQ 核心启动                            │
│    - 启动 Mnesia/Khepri 数据库                                  │
│    - 加载插件                                                   │
│    - 启动监听器                                                 │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 4. Change boot state to `core_started`                          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 5. Change boot state to `ready`                                 │
│    systemd:notify([ready, {status, "Initialized"}])             │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ systemd 收到 READY=1，标记服务为 active (running)               │
└─────────────────────────────────────────────────────────────────┘
```

### 5.4 查看启动日志

```bash
# 查看启动状态变化
$ grep "Change boot state to" /var/log/rabbitmq/rabbit@rabbitmq.log
2025-01-04 07:59:46.747822 [debug] <0.237.0> Change boot state to `core_started`
2025-01-04 07:59:46.898118 [debug] <0.477.0> Change boot state to `ready`

# 查看 systemd 状态
$ systemctl status rabbitmq-server.service
● rabbitmq-server.service - RabbitMQ broker
   Loaded: loaded (/usr/lib/systemd/system/rabbitmq-server.service; disabled)
   Active: active (running) since Sat 2025-01-04 01:41:22 EST; 12min ago
 Main PID: 69505 (beam.smp)
   Status: "Initialized"
```

### 5.5 手动启动（非 systemd）

```bash
# 设置插件路径
export ERL_LIBS=/usr/lib/rabbitmq/lib/rabbitmq_server-3.9.21/plugins

# 启动 Erlang VM 运行 RabbitMQ
ERL_LIBS=/usr/lib/rabbitmq/lib/rabbitmq_server-3.9.21/plugins erl \
    -noinput -s rabbit boot -boot start_sasl
```

---

## 6. 实践操作指南

### 6.1 安装服务

```bash
# 1. 复制程序到系统目录
sudo cp helloworld.sh /usr/local/bin/
sudo chmod +x /usr/local/bin/helloworld.sh

# 2. 复制服务单元文件
sudo cp helloworld.service /etc/systemd/system/

# 3. 重新加载 systemd 配置
sudo systemctl daemon-reload
```

### 6.2 管理服务

```bash
# 启动服务
sudo systemctl start helloworld

# 查看状态
sudo systemctl status helloworld

# 停止服务
sudo systemctl stop helloworld

# 重启服务
sudo systemctl restart helloworld

# 开机自启
sudo systemctl enable helloworld

# 禁用开机自启
sudo systemctl disable helloworld
```

### 6.3 查看日志

```bash
# 查看服务日志
journalctl -u helloworld

# 实时跟踪日志
journalctl -u helloworld -f

# 查看最近 100 行
journalctl -u helloworld -n 100

# 查看启动以来的日志
journalctl -u helloworld -b
```

### 6.4 调试通知

```bash
# 手动测试通知（需要在服务环境中）
NOTIFY_SOCKET=/run/systemd/notify systemd-notify --ready

# 查看 NOTIFY_SOCKET 环境变量
echo $NOTIFY_SOCKET
```

---

## 7. 对比总结

| 语言 | 通知函数 | 依赖 | 复杂度 |
|------|---------|------|--------|
| **Shell** | `systemd-notify` | 无 | 最简单 |
| **C** | `sd_notify()` | systemd-devel | 简单 |
| **Python** | `systemd.daemon.notify()` | python-systemd | 简单 |
| **Erlang** | `systemd:notify/1` | systemd (Hex) | 中等 |

### 服务单元配置要点

```ini
[Service]
Type=notify          # 必须设置为 notify
NotifyAccess=all     # Erlang/多进程应用需要设置
TimeoutStartSec=90   # 给足启动超时时间
```

---

## 8. 参考资源

- [systemd 官方文档](https://www.freedesktop.org/software/systemd/man/)
- [sd_notify(3) 手册](https://www.freedesktop.org/software/systemd/man/sd_notify.html)
- [RabbitMQ 安装指南](https://www.rabbitmq.com/install-rpm.html)
- [Erlang systemd 库](https://github.com/hauleth/systemd)
- [示例代码仓库](https://github.com/hzsunzixiang/rabbitmq-server-debug/tree/main/book/systemd)
