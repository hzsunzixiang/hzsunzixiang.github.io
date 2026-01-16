---
title: Erlang 进程链接与监视器（中文版）
date: 2026-01-16 10:30:00
tags:
  - Erlang
  - OTP
  - 并发编程
  - 进程链接
  - 监视器
categories:
  - 技术
  - Erlang
---

# 用于监督的链接和监视器

当进程相互依赖时，我们基本上有两种选择：让它们同生共死，或者让一个进程在另一个进程崩溃时得到通知。第三章讨论的 Erlang "任其崩溃"哲学，使我们倾向于后一种选择。

链接的进程形成一个组，默认情况下会一起崩溃。如果其中一个链接的进程异常退出，其组中的所有其他进程都会收到 `EXIT` 信号并一起崩溃。监视器允许一个进程监视另一个进程，而不会自动被耦合到同一个组中。

<!-- more -->

## 链接（Links）

链接是双向的。如果两个进程被链接，它们被称为属于同一个链接集。如果其中一个异常终止，退出信号会传播到属于其链接集的所有进程。

设置两个进程之间的链接，可以使用以下 BIF：

- `link(Pid)` 在调用进程和进程标识符为 `Pid` 的进程之间建立双向链接。

- `spawn_link(Module, Function, Args)` 原子地生成并链接到一个进程。这相当于生成一个进程并链接到它，但是以原子方式完成，因此不存在生成的进程在链接建立之前死亡的风险。

- `unlink(Pid)` 移除调用进程和进程标识符为 `Pid` 的进程之间的任何链接。

### 示例

```erlang
1> Pid = spawn(fun() -> receive X -> X end end).
<0.25.0>
2> link(Pid).
true
3> exit(Pid, kill).
true
** exception exit: killed
```

在这个例子中，链接到生成的进程然后发送 kill 信号会导致生成的进程和 shell 都崩溃。

## 捕获退出（Trapping Exits）

进程可以通过设置以下标志来捕获退出信号：

```erlang
process_flag(trap_exit, true)
```

当一个进程正在捕获退出时，退出信号会被转换成 `{'EXIT', Pid, Reason}` 形式的消息，可以在普通的 receive 语句中处理。

### 示例

```erlang
1> process_flag(trap_exit, true).
false
2> Pid = spawn_link(fun() -> receive X -> X end end).
<0.35.0>
3> exit(Pid, reason).
true
4> receive X -> X end.
{'EXIT',<0.35.0>,reason}
```

## 表 2-1. 传播语义

| 原因            | 捕获退出 (`trap_exit=true`)                | 不捕获退出                    |
|-----------------|-------------------------------------------|------------------------------|
| `normal`        | 接收 `{'EXIT', Pid, normal}`              | 无事发生                      |
| `kill`          | 以原因 `killed` 终止                       | 以原因 `killed` 终止          |
| 其他            | 接收 `{'EXIT', Pid, Other}`               | 以原因 `Other` 终止           |

## 监视器（Monitors）

与链接不同，监视器是单向的。当被监视的进程终止时，监视进程会在其邮箱中收到一条 `{'DOWN', Reference, process, Pid, Reason}` 消息，但不会向相反方向发送信号。

使用 BIF 设置监视器：

```erlang
Reference = monitor(process, Pid)
```

如果设置了多个监视器，`Reference` 可用于标识特定的监视器。

要移除监视器，使用：

```erlang
demonitor(Reference)
```

### 监视器与链接对比

| 特性                  | 链接              | 监视器             |
|-----------------------|------------------|-------------------|
| 方向性                | 双向              | 单向              |
| 默认行为              | 一起崩溃          | 退出时发送消息     |
| 信号传播              | 双向              | 单向              |
| 设置函数              | `link/1`         | `monitor/2`       |
| 移除函数              | `unlink/1`       | `demonitor/1`     |

### 示例

```erlang
1> Pid = spawn(fun() -> receive X -> X end end).
<0.25.0>
2> Ref = monitor(process, Pid).
#Ref<0.0.0.26>
3> exit(Pid, reason).
true
4> receive X -> X end.
{'DOWN',#Ref<0.0.0.26>,process,<0.25.0>,reason}
```

## 总结

- **链接** 在进程之间创建双向连接。当一个进程死亡时，所有链接的进程都会收到退出信号。
- **监视器** 创建单向观察。当被监视的进程终止时，监视进程会收到 `'DOWN'` 消息。
- 使用 `process_flag(trap_exit, true)` 将退出信号转换为消息。
- `kill` 原因无法被捕获，总是会终止进程。
