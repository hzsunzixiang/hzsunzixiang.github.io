---
title: Erlang 知识点汇总
date: 2024-01-16
tags:
  - Erlang
  - 分布式系统
  - RabbitMQ
categories:
  - 技术
  - Erlang
---

# Erlang 知识点汇总

本文汇总了 Erlang 开发中的常用知识点，包括基础语法、分布式节点通信、调试技巧等。

<!-- more -->

## 基础语法

### 常用函数

- `list_to_integer` - 列表转整数
- `byte_size` - 获取字节大小
- `{packet, http_bin}` - HTTP 二进制包选项

### 定义和加载 Record

```erlang
% 定义 record
rd(factorial, {nodeName, comment, createdOn}).

% 加载 record
rr(amqp_connection).
```

参考: [Erlang -- shell](https://www.erlang.org/doc/man/shell)

### 创建 Mnesia 表

```erlang
rd(factorial, {nodeName, comment, createdOn}).

% 内存表
Result = mnesia:create_table(factorial, [
    {attributes, record_info(fields, factorial)}, 
    {type, bag}, 
    {ram_copies, [node()]}
]).

% 磁盘表
Result2 = mnesia:create_table(factorial1, [
    {attributes, record_info(fields, factorial)}, 
    {type, bag}, 
    {disc_copies, [node()]}
]).
```

### 查看 Mnesia 表信息

![Mnesia表信息](/images/erlang_basic/image2.png)

---

## 循环与列表操作

### 生成数组

```erlang
[X || X <- lists:seq(1,100), X rem 2 == 0].
```

### 遍历数组

```erlang
lists:foldl(fun(X, Sum) -> X + Sum end, 0, [X || X <- lists:seq(1,100)]).
```

### 调用100次某函数

```erlang
lists:foldl(fun(X, Sum) -> database_logic:storeDB(node(), X) end, 0, [X || X <- lists:seq(1,100)]).
```

### lists:takewhile

![lists:takewhile](/images/erlang_basic/image46.png)

---

## Module Attributes

参考: [Erlang -- Modules](https://www.erlang.org/doc/reference_manual/modules)

![Module Attributes](/images/erlang_basic/image3.png)

![Module Attributes 示例](/images/erlang_basic/image4.png)

### Module Attributes with Tuple

```erlang
-module(rabbit).
-author("TutorialPoint").
-version("1.0").
-module_description("This is an example module.").

-rabbit_boot_step({pre_boot, [{description, "rabbit boot start"}]}).

-rabbit_boot_step({codec_correctness_check,
    [{description, "codec correctness check"},
     {mfa, {rabbit_binary_generator, check_empty_frame_size, []}},
     {requires, pre_boot},
     {enables, external_infrastructure}]}).

-export([start/0]).

start() ->
    io:fwrite("Hello World\n"),
    ModuleInfo = rabbit:module_info(),
    Attributes = rabbit:module_info(attributes),
    io:format("~p~n", [Attributes]),
    finish.
```

---

## 分布式节点

### Hidden Node

![Hidden Node](/images/erlang_basic/image1.png)

参考: [Erlang -- Distributed Erlang](https://www.erlang.org/doc/reference_manual/distributed.html)

### 远程连接 MQ 节点

#### 使用 net_kernel:connect_node

参考: [net_kernel:connect_node/1](https://blog.differentpla.net/blog/2013/11/07/net-kernel-connect-node-1-returns-ignored/)

需要给节点起个名字：

```bash
erl -sname apple
```

```erlang
(apple@kvm_10_21_1_163)1> net_kernel:connect_node('rabbit@kvm_10_20_1_60').
true
```

#### 使用 net_adm:ping

注意是 `-sname` 在生效，不用 `-name`，能够在远程节点生效。

![net_adm:ping](/images/erlang_basic/image5.png)

#### 使用 rpc:call

![rpc:call](/images/erlang_basic/image6.png)

#### 使用消息发送 !

![消息发送](/images/erlang_basic/image7.png)

![消息发送示例](/images/erlang_basic/image8.png)

#### 使用 monitor

![monitor 示例1](/images/erlang_basic/image9.png)

![monitor 示例2](/images/erlang_basic/image10.png)

![monitor 示例3](/images/erlang_basic/image11.png)

![monitor 示例4](/images/erlang_basic/image12.png)

**case1**: 节点不存在
**case2**: 节点存在，服务不存在，建立连接
**case3**: 远程节点和进程都存在

![monitor case](/images/erlang_basic/image13.png)

---

## Observer 远程观察

### 步骤

1. **新起一个节点**

![新起节点](/images/erlang_basic/image14.png)

2. **启动 observer**

![启动observer](/images/erlang_basic/image15.png)

3. **连接其他 node**

在本机上即使不连接，能看到其他节点的时候也可以直接切换。
在远程，必须先连接上，然后再切换。

![连接节点](/images/erlang_basic/image16.png)

或者用命令行：

![命令行连接](/images/erlang_basic/image17.png)

4. **切换到其他节点**

![切换节点](/images/erlang_basic/image18.png)

5. **查看节点信息**

![节点信息](/images/erlang_basic/image19.png)

---

## 调试 RabbitMQ 节点

### 节点无法连接的调试

- 查看防火墙，如果有，需要关闭

![防火墙检查1](/images/erlang_basic/image20.png)

![防火墙检查2](/images/erlang_basic/image21.png)

![防火墙检查3](/images/erlang_basic/image22.png)

### 初步启动

![初步启动](/images/erlang_basic/image23.png)

### 停止

```erlang
(rabbit@centos7-mq1)2> rabbit:stop().
ok
```

![停止](/images/erlang_basic/image24.png)

### 再次开启

```erlang
(rabbit@centos7-mq1)4> rabbit:start().
ok
```

### Node 位置无关性

![位置无关性1](/images/erlang_basic/image25.png)

![位置无关性2](/images/erlang_basic/image26.png)

---

## 代码加载与模块管理

### 列出已加载的库（符号表）

```erlang
code:all_loaded().
rp(code:all_loaded()).
```

![已加载模块](/images/erlang_basic/image27.png)

### 加载模块

```erlang
(apple@rabbitmq-1)11> l(ping).
```

### 查看包依赖

```erlang
% 直接启动试试
application:ensure_all_started(systemd).
{ok,[enough,systemd]}
```

- 从 `rebar.config` 中查看: `{deps, [enough]}.`
- `rebar3 deps`
- `rebar3 tree`

![依赖树](/images/erlang_basic/image28.png)

- `make list-deps`

---

## 把节点变成分布式节点

参考: [Erlang -- net_kernel](https://www.erlang.org/doc/man/net_kernel.html#start-1)

```erlang
net_kernel:start([foo, shortnames]).
```

### 启动之前

![启动前](/images/erlang_basic/image30.png)

### 启动之后

![启动后1](/images/erlang_basic/image31.png)

![启动后2](/images/erlang_basic/image32.png)

多出来的部分：

![多出部分](/images/erlang_basic/image33.png)

![分布式详情](/images/erlang_basic/image34.png)

源码位置: `/home/ericksun/program/otp-25.0.4/lib/erlang/lib/kernel-8.4.2/src/erl_distribution.erl`

![源码1](/images/erlang_basic/image35.png)

![源码2](/images/erlang_basic/image36.png)

---

## 异常处理

![异常类型](/images/erlang_basic/image37.png)

### catch 之后不会结束当前进程

![catch示例1](/images/erlang_basic/image38.png)

![catch示例2](/images/erlang_basic/image39.png)

![catch示例3](/images/erlang_basic/image40.png)

---

## begin..end

参考: [What is begin...end in Erlang used for?](https://stackoverflow.com/questions/20125277/what-is-begin-end-in-erlang-used-for)

![begin..end 1](/images/erlang_basic/image41.png)

![begin..end 2](/images/erlang_basic/image42.png)

![begin..end 3](/images/erlang_basic/image43.png)

![begin..end 4](/images/erlang_basic/image44.png)

---

## 进程通信

### $ dollar sign

![dollar sign](/images/erlang_basic/image45.png)

### 发送信息到远程节点

参考: [Erlang -- Concurrent Programming](https://www.erlang.org/doc/getting_started/conc_prog.html#message-passing)

```erlang
{pong, Pong_Node} ! {ping, self()},
```

使用 tuple `{registered_name, node_name}` 代替单独的 `registered_name`。

![远程发送](/images/erlang_basic/image47.png)

### 进程的注册和获取

![进程注册](/images/erlang_basic/image48.png)

### 获取远程进程 PID

通过 register 方式：

![获取远程PID 1](/images/erlang_basic/image49.png)

![获取远程PID 2](/images/erlang_basic/image50.png)

![获取远程PID 3](/images/erlang_basic/image51.png)

### 直接用远程 PID 发送消息

io_request 发送到远程，无法直接执行打印的时候，放到消息队列：

![io_request](/images/erlang_basic/image52.png)

发给 group_leader 直接执行，不会放到进程的消息队列中：

![group_leader](/images/erlang_basic/image53.png)

### Observer 观察消息队列

![消息队列1](/images/erlang_basic/image54.png)

![消息队列2](/images/erlang_basic/image55.png)

---

## 模式匹配

```erlang
calls(TSpecs = [_|_], {Max, Time}, Opts)
```

Pattern matching 的重要用途：
- 选择控制流分支
- 执行变量赋值（绑定）
- 解构数据结构（选择和提取部分）

![模式匹配](/images/erlang_basic/image56.png)

### 函数参数中的模式匹配

![函数参数模式匹配](/images/erlang_basic/image57.png)

`#state{...}=State` 是别名模式：同时匹配和赋值。

---

## 短路求值

参考: [Erlang -- Expressions](https://www.erlang.org/doc/reference_manual/expressions.html)

```erlang
Expr1 orelse Expr2
Expr1 andalso Expr2
```

![短路求值1](/images/erlang_basic/image58.png)

修改后只允许一个实例存在：

![短路求值2](/images/erlang_basic/image59.png)

---

## 常用工具函数

### net_adm:localhost/0

```erlang
1> net_adm:localhost().
"centos7-mq1"
```

### 列表打印完全

`rp(Term)` - 使用 shell 已知的 record 定义打印 term，不限制深度。

```erlang
rp(code:all_loaded()).
```

参考: [Erlang -- shell](https://www.erlang.org/doc/man/shell.html)

### 查看 Event 中所有的 Handler

**针对旧版 error_logger：**
```erlang
gen_event:which_handlers(error_logger).
```

**针对新的 logger 模块（OTP 21.0+）：**
```erlang
logger:get_handler_config().
```

---

## 相关资源

- [Erlang -- erlang](https://www.erlang.org/doc/man/erlang.html)
- [Erlang -- logger](https://www.erlang.org/doc/man/logger.html)
- [Erlang -- code](https://www.erlang.org/doc/man/code.html)
- [Learn You Some Erlang](https://learnyousomeerlang.com/)
- [Core Erlang by Example](https://www.erlang.org/blog/core-erlang-by-example/)
