---
title: 第四章：通用型服务器 gen_server
date: 2024-01-18
updated: 2026-01-15
categories:
  - 技术
  - Erlang/OTP
tags:
  - Erlang
  - OTP
  - gen_server
  - 并发编程
toc: true
---

本文介绍 Erlang/OTP 中的通用型服务器行为模式 gen_server，这是 OTP 框架中最常用的 behavior 之一。

<!-- more -->

## gen_server 概述

gen_server 是 OTP 提供的通用服务器行为模式，它封装了客户端-服务器模型中的常见模式，让开发者可以专注于业务逻辑而不是底层的进程通信细节。

使用 gen_server 的优势：
- 标准化的服务器实现模式
- 内置的错误处理机制
- 支持同步和异步消息传递
- 与 OTP 监督树无缝集成

## behavior 指令

要实现一个 gen_server，需要在模块中声明 behavior：

```erlang
-module(my_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server 回调函数
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
```

## 启动一个 server

gen_server 提供了多种启动函数：

```erlang
%% 启动一个匿名服务器
gen_server:start_link(Module, Args, Options)

%% 启动一个命名服务器（本地注册）
gen_server:start_link({local, Name}, Module, Args, Options)

%% 启动一个命名服务器（全局注册）
gen_server:start_link({global, Name}, Module, Args, Options)
```

启动时会调用 `init/1` 回调函数：

```erlang
init(Args) ->
    %% 初始化服务器状态
    State = #state{},
    {ok, State}.
```

`init/1` 可以返回：
- `{ok, State}` - 正常启动
- `{ok, State, Timeout}` - 启动并设置超时
- `{ok, State, hibernate}` - 启动并进入休眠
- `{stop, Reason}` - 启动失败
- `ignore` - 忽略启动

## 消息传递

gen_server 支持多种消息传递方式：

![gen_server 消息传递流程](/images/erlang_otp/gen_server_flow.svg)

### 同步式消息传递

使用 `gen_server:call/2,3` 发送同步请求，客户端会阻塞等待响应：

```erlang
%% 客户端调用
Result = gen_server:call(ServerRef, Request),
Result = gen_server:call(ServerRef, Request, Timeout).

%% 服务器端处理
handle_call(Request, From, State) ->
    Reply = do_something(Request),
    {reply, Reply, State}.
```

`handle_call/3` 的参数：
- `Request` - 客户端发送的请求
- `From` - 调用者信息 `{Pid, Tag}`
- `State` - 当前服务器状态

### 异步式消息传递

使用 `gen_server:cast/2` 发送异步请求，客户端不等待响应：

```erlang
%% 客户端调用
ok = gen_server:cast(ServerRef, Msg).

%% 服务器端处理
handle_cast(Msg, State) ->
    NewState = process_msg(Msg, State),
    {noreply, NewState}.
```

### 其他消息

服务器可能收到不是通过 `call` 或 `cast` 发送的消息，这些消息由 `handle_info/2` 处理：

```erlang
handle_info(Info, State) ->
    case Info of
        timeout ->
            %% 处理超时
            {noreply, State};
        {nodedown, Node} ->
            %% 处理节点断开
            {noreply, State};
        _Other ->
            %% 忽略未知消息
            {noreply, State}
    end.
```

### 未处理的消息

良好的实践是在 `handle_info/2` 中记录未预期的消息：

```erlang
handle_info(Msg, State) ->
    error_logger:warning_msg("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.
```

## 同步客户端

实现一个完整的同步客户端 API：

```erlang
-module(kv_store).
-behaviour(gen_server).

%% API
-export([start_link/0, get/1, put/2, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {store = #{}}).

%%% API 函数

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

put(Key, Value) ->
    gen_server:call(?MODULE, {put, Key, Value}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

%%% 回调函数

init([]) ->
    {ok, #state{}}.

handle_call({get, Key}, _From, State = #state{store = Store}) ->
    Reply = maps:get(Key, Store, undefined),
    {reply, Reply, State};

handle_call({put, Key, Value}, _From, State = #state{store = Store}) ->
    NewStore = maps:put(Key, Value, Store),
    {reply, ok, State#state{store = NewStore}};

handle_call({delete, Key}, _From, State = #state{store = Store}) ->
    NewStore = maps:remove(Key, Store),
    {reply, ok, State#state{store = NewStore}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

## 终止

服务器终止时会调用 `terminate/2`：

```erlang
terminate(Reason, State) ->
    %% 清理资源
    cleanup(State),
    ok.
```

终止原因可能是：
- `normal` - 正常终止
- `shutdown` - 监督者要求关闭
- `{shutdown, Term}` - 带额外信息的关闭
- 其他值 - 异常终止

## 调用超时

`gen_server:call/3` 支持设置超时时间：

```erlang
%% 默认超时 5000ms
gen_server:call(Server, Request).

%% 自定义超时
gen_server:call(Server, Request, 10000).  %% 10秒

%% 无限等待
gen_server:call(Server, Request, infinity).
```

超时后客户端会收到 `exit` 信号：

```erlang
try
    gen_server:call(Server, Request, 5000)
catch
    exit:{timeout, _} ->
        {error, timeout}
end.
```

## 死锁

避免 gen_server 死锁的几个原则：

1. **不要在回调函数中调用自己**：

```erlang
%% 错误示例 - 会导致死锁
handle_call(get_twice, _From, State) ->
    Result1 = gen_server:call(self(), get),  %% 死锁！
    {reply, Result1, State}.
```

2. **避免循环调用**：

```erlang
%% Server A 调用 Server B，Server B 又调用 Server A
%% 这会导致死锁
```

3. **使用 cast 代替 call 打破循环**

## 通用型 server 的超时问题

可以在回调函数中返回超时值：

```erlang
init([]) ->
    {ok, #state{}, 5000}.  %% 5秒后触发 timeout

handle_call(Request, _From, State) ->
    {reply, ok, State, 5000}.  %% 重置超时

handle_info(timeout, State) ->
    %% 处理超时，例如执行定期任务
    do_periodic_work(),
    {noreply, State, 5000}.  %% 继续等待下一个超时
```

## 使 behavior 休眠

当服务器空闲时，可以让它进入休眠状态以释放内存：

```erlang
handle_call(Request, _From, State) ->
    {reply, ok, State, hibernate}.

handle_cast(Msg, State) ->
    {noreply, State, hibernate}.

handle_info(Info, State) ->
    {noreply, State, hibernate}.
```

休眠时，进程会进行垃圾回收并最小化内存占用。收到新消息时自动唤醒。

## 全局化

使用全局名称注册 gen_server：

```erlang
%% 全局注册启动
gen_server:start_link({global, my_global_server}, ?MODULE, [], []).

%% 全局调用
gen_server:call({global, my_global_server}, Request).
gen_server:cast({global, my_global_server}, Msg).
```

也可以使用 `via` 元组与自定义注册表：

```erlang
%% 使用 gproc 作为注册表
gen_server:start_link({via, gproc, {n, l, my_server}}, ?MODULE, [], []).
```

## 链接 behavior

gen_server 通常与监督者链接使用：

```erlang
%% 在监督者中定义子规范
init([]) ->
    Children = [
        #{id => my_server,
          start => {my_server, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [my_server]}
    ],
    {ok, {#{strategy => one_for_one}, Children}}.
```

## gen_server 生命周期

![gen_server 生命周期](/images/erlang_otp/gen_server_lifecycle.svg)

## 总结

gen_server 是 Erlang/OTP 中最重要的 behavior 之一：

| 特性 | 说明 |
|------|------|
| 同步调用 | `call/2,3` + `handle_call/3` |
| 异步调用 | `cast/2` + `handle_cast/2` |
| 其他消息 | `handle_info/2` |
| 生命周期 | `init/1` → 运行 → `terminate/2` |
| 超时处理 | 回调返回值中指定 |
| 休眠 | `hibernate` 选项 |

掌握 gen_server 是学习 OTP 的基础，它为构建健壮的并发应用提供了标准化的模式。

## 接下来是什么

下一章我们将学习 **gen_statem**（有限状态机）行为模式，它适用于需要状态转换的场景。

---

## 相关资源

- [Erlang gen_server 官方文档](https://www.erlang.org/doc/man/gen_server.html)
- [OTP Design Principles](https://www.erlang.org/doc/design_principles/gen_server_concepts.html)
