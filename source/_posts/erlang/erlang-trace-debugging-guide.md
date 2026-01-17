---
title: Erlang Trace 调试技术详解
date: 2026-01-16 23:00:00
categories:
  - technology
  - Erlang
tags:
  - Erlang
  - 调试
  - trace
  - dbg
  - RabbitMQ
  - 源码分析
---

# Erlang Trace 调试技术详解

## 概述

与 C/C++ 的 GDB 单步调试和 Java IDE（Eclipse/IntelliJ IDEA）的断点调试不同，Erlang 采用基于进程的 trace 机制进行调试。本文深入介绍 Erlang 的 trace 调试技术，这是 RabbitMQ 源码分析的重要工具。

## 参考资料

### 推荐书籍

- **[Erlang in Anger](https://www.erlang-in-anger.com/)** - 生产环境 Erlang 调试实战指南
- **[Erlang Programming](https://www.amazon.com/Erlang-Programming-Concurrent-Approach-Development/dp/0596518188)** - Francesco Cesarini, Simon Thompson

### 技术文档

- [dbg.pdf](https://www.erlang-factory.com/upload/presentations/316/dbg[1].pdf) - Erlang Factory 演讲
- [Declarative debugging of concurrent Erlang programs](https://www.academia.edu/113957930/Declarative_debugging_of_concurrent_Erlang_programs)
- [Trace Analysis of Erlang Programs](https://www.researchgate.net/publication/220178306_Trace_Analysis_of_Erlang_Programs)
- [Profiling and Tracing for all with Xprof](https://esl-conf-static.s3.eu-central-1.amazonaws.com/media/files/000/000/725/original/Peter_Gomori_-_Profiling_and_Tracing_for_all_with_Xprof.pdf)

---

## Erlang 调试工具总览

Erlang 提供了多层次的调试工具：

| 工具 | 层级 | 特点 |
|------|------|------|
| `erlang:trace/3` | 底层 BIF | 最基础，所有工具的基础 |
| `erlang:trace_pattern/3` | 底层 BIF | 函数调用追踪 |
| `dbg` | 标准库 | 基于 trace 的封装，更易用 |
| `redbug` | 第三方 | 生产环境安全的追踪工具 |
| `recon_trace` | 第三方 | 专注函数调用追踪 |

---

## Trace 基础概念

### 可追踪的事件类型

Trace 可以追踪三大类事件：

| 事件类型 | 工具 | Trace Flag |
|----------|------|------------|
| 进程相关活动和消息传递 | trace, dbg, redbug | `all`, `send`, `'receive'`, `procs`, `running` |
| 垃圾回收和内存使用 | trace, dbg, redbug | `garbage_collection`, `timestamp` |
| 全局和本地函数调用 | trace + trace_pattern, dbg, recon_trace | `call`, `arity`, `return_to` |

### Trace 消息格式

跟踪事件以消息形式发送：

```erlang
{trace, Pid, Tag, Data1 [,Data2]}
```

其中 `[,Data2]` 是可选字段，依赖于跟踪消息类型。

---

## erlang:trace/3 API 详解

### 基本语法

```erlang
erlang:trace(PidSpec, Bool, TraceFlags) -> integer()
```

- **PidSpec**: 要追踪的进程（`all`, `new`, `existing`, `Pid`）
- **Bool**: `true` 启用追踪，`false` 禁用
- **TraceFlags**: 追踪标志列表
- **返回值**: 被追踪的进程数量

### 重要注意事项

1. **Tracer 进程不能被追踪**
   ```erlang
   %% 错误：tracer 不能追踪自己
   erlang:trace(self(), true, [send]).  %% badarg error
   ```

2. **指定其他进程接收追踪消息**
   ```erlang
   %% 将追踪消息发送到指定进程
   erlang:trace(all, true, [send, {tracer, TracerPid}]).
   ```

3. **终端需要 flush() 查看消息**
   ```erlang
   %% 追踪消息在 mailbox 中，需要 flush 显示
   1> erlang:trace(Pid, true, [send]).
   2> flush().
   ```

---

## 进程和消息追踪

### send 和 'receive' 追踪

#### send 追踪

```erlang
%% 启用发送追踪
erlang:trace(Pid, true, [send]).

%% 追踪消息格式
{trace, PidPort, send, Msg, To}
{trace, PidPort, send_to_non_existing_process, Msg, To}
```

#### 'receive' 追踪

```erlang
%% 启用接收追踪（注意：'receive' 需要引号，因为是保留字）
erlang:trace(Pid, true, ['receive']).

%% 追踪消息格式
{trace, PidPort, 'receive', Msg}
```

### 进程生命周期追踪 (procs)

```erlang
%% 追踪进程创建、退出等事件
erlang:trace(all, true, [procs]).

%% 配合 set_on_spawn 追踪新创建的进程
erlang:trace(all, true, [set_on_spawn, procs]).
```

### running 追踪

```erlang
%% 追踪进程调度（运行/挂起）
erlang:trace(Pid, true, [running, procs]).
```

### 垃圾回收追踪

```erlang
%% 追踪 GC 事件，带时间戳
erlang:trace(Pid, true, [garbage_collection, timestamp]).
```

**注意**: 占用内存太小可能触发不到垃圾回收。

---

## 函数调用追踪

函数追踪需要 `erlang:trace/3` 和 `erlang:trace_pattern/3` 配合使用。

### 基本原理

| 函数 | 作用 |
|------|------|
| `erlang:trace/3` | 定义追踪哪些**进程** |
| `erlang:trace_pattern/3` | 定义追踪哪些**函数** |

只有当**被追踪的进程**调用**被追踪的函数**时，才会产生追踪事件。

### trace_pattern 基本语法

```erlang
erlang:trace_pattern(MFA, MatchSpec, FlagList) -> integer()
```

- **MFA**: `{Module, Function, Arity}` 或 `{Module, '_', '_'}`
- **MatchSpec**: 匹配规则（`true`, `[]`, 或详细规则）
- **FlagList**: `[global]`, `[local]`, `[meta]` 等

### 基本示例

```erlang
%% 1. 启用进程的函数调用追踪
erlang:trace(all, true, [call]).

%% 2. 指定追踪的函数（必须先 load module）
erlang:trace_pattern({mymodule, myfunction, 2}, true, [local]).

%% 注意：模块必须先加载！
l(mymodule).  %% 或 code:ensure_loaded(mymodule)
```

### global vs local

| 选项 | 说明 |
|------|------|
| `global` | 默认值，只追踪外部调用（跨模块调用） |
| `local` | 追踪所有调用，包括模块内部调用 |

```erlang
%% 追踪所有调用（包括内部调用）
erlang:trace_pattern({mymodule, '_', '_'}, true, [local]).

%% 只追踪外部调用
erlang:trace_pattern({mymodule, '_', '_'}, true, [global]).
```

### return_to 追踪

追踪函数返回到哪里（只对 `local` 有效）：

```erlang
%% 启用 return_to
erlang:trace(all, true, [call, return_to]).
erlang:trace_pattern({mymodule, '_', '_'}, true, [local]).

%% 追踪消息格式
{trace, Pid, return_to, {M, F, A}}
```

**注意**: `return_to` 无法获取函数返回值！

### return_trace 获取返回值

要获取函数返回值，使用 MatchSpec 中的 `{return_trace}`：

```erlang
%% 使用 return_trace 获取返回值
erlang:trace(all, true, [call]).
erlang:trace_pattern({mymodule, myfunction, 1}, [{'_', [], [{return_trace}]}], [local]).

%% 追踪消息格式
{trace, Pid, call, {M, F, Args}}
{trace, Pid, return_from, {M, F, Arity}, ReturnValue}
```

### arity 选项

```erlang
%% 使用 arity：只显示参数个数，不显示参数内容
erlang:trace(all, true, [call, arity]).

%% 不使用 arity：显示完整参数
erlang:trace(all, true, [call]).
```

使用 `arity` 可以减少追踪消息大小。

---

## Match Specifications

Match Specifications 是强大的过滤和操作机制。

### 基本结构

```erlang
MatchSpec = [{MatchHead, MatchConditions, MatchBody}]
```

### 使用 dbg:fun2ms/1 生成

```erlang
%% 使用 fun2ms 生成 MatchSpec
1> dbg:fun2ms(fun([A, B]) when A > 10 -> return_trace() end).
[{['$1','$2'],[{'>','$1',10}],[{return_trace}]}]
```

### 常用 MatchSpec 示例

```erlang
%% 追踪所有调用并返回值
erlang:trace_pattern({M, F, A}, [{'_', [], [{return_trace}]}], [local]).

%% 追踪特定参数值
erlang:trace_pattern({M, F, 1}, [{['$1'], [{'==', '$1', foo}], []}], [local]).

%% 追踪并打印消息
erlang:trace_pattern({M, F, A}, [{'_', [], [{message, {process_dump}}]}], [local]).
```

---

## dbg 模块

`dbg` 是基于 `erlang:trace/3` 的高级封装，使用更简便。

### 基本使用流程

```erlang
%% 1. 启动 tracer
dbg:tracer().

%% 2. 设置追踪的进程
dbg:p(all, c).  %% all 进程，c = call

%% 3. 设置追踪的函数
dbg:tpl(mymodule, myfunction, x).  %% x = 所有 arity

%% 4. 停止追踪
dbg:stop_clear().
```

### 常用命令

| 命令 | 说明 |
|------|------|
| `dbg:tracer()` | 启动默认 tracer |
| `dbg:p(PidSpec, Flags)` | 设置进程追踪 |
| `dbg:tp(M, F, A, MS)` | 追踪全局函数调用 |
| `dbg:tpl(M, F, A, MS)` | 追踪本地+全局函数调用 |
| `dbg:ctp(M, F, A)` | 清除追踪模式 |
| `dbg:stop()` | 停止 tracer |
| `dbg:stop_clear()` | 停止并清除所有追踪 |

### dbg:p/2 的 Flags

| Flag | 说明 |
|------|------|
| `c` | call - 函数调用 |
| `s` | send - 发送消息 |
| `r` | receive - 接收消息 |
| `m` | messages - send + receive |
| `p` | procs - 进程事件 |
| `sos` | set_on_spawn |
| `sol` | set_on_link |

### 实际示例

```erlang
%% 追踪 lists:seq/2 的调用
1> dbg:tracer().
{ok,<0.90.0>}

2> dbg:p(all, c).
{ok,[{matched,nonode@nohost,26}]}

3> dbg:tpl(lists, seq, x).
{ok,[{matched,nonode@nohost,2}]}

4> lists:seq(1, 5).
(<0.84.0>) call lists:seq(1,5)
[1,2,3,4,5]

5> dbg:stop_clear().
ok
```

### 带返回值的追踪

```erlang
%% 使用 dbg:fun2ms 生成带 return_trace 的 MatchSpec
1> dbg:tracer().
2> dbg:p(all, c).
3> dbg:tpl(lists, seq, dbg:fun2ms(fun(_) -> return_trace() end)).
4> lists:seq(1, 3).
(<0.84.0>) call lists:seq(1,3)
(<0.84.0>) returned from lists:seq/2 -> [1,2,3]
[1,2,3]
```

---

## recon_trace

`recon_trace` 是生产环境安全的追踪工具，来自 `recon` 库。

### 特点

- 自动限制追踪消息数量，防止系统过载
- 只能追踪函数调用，不能追踪消息
- 生产环境推荐使用

### 基本使用

```erlang
%% 追踪 10 次调用
recon_trace:calls({lists, seq, 2}, 10).

%% 带返回值
recon_trace:calls({lists, seq, 2}, 10, [{scope, local}]).

%% 追踪特定进程
recon_trace:calls({M, F, A}, 10, [{pid, Pid}]).
```

### 重要提示

使用 `recon_trace` 前，需要预加载所有要追踪的模块：

```erlang
%% 加载所有 beam 文件对应的模块
LP = fun() -> 
    [code:ensure_loaded(list_to_atom(filename:rootname(filename:basename(F)))) 
     || P <- code:get_path(), F <- filelib:wildcard(P ++ "/*.beam")] 
end.
LP().
```

---

## 实战技巧

### 1. 追踪 RabbitMQ 函数调用

```erlang
%% 追踪 rabbit_channel 模块
1> dbg:tracer().
2> dbg:p(all, c).
3> dbg:tpl(rabbit_channel, handle_cast, x).
4> %% 执行相关操作...
5> dbg:stop_clear().
```

### 2. 追踪消息传递

```erlang
%% 追踪特定进程的消息
1> Pid = whereis(rabbit_reader).
2> erlang:trace(Pid, true, [send, 'receive']).
3> flush().  %% 查看追踪消息
4> erlang:trace(Pid, false, [send, 'receive']).  %% 停止追踪
```

### 3. 生产环境安全追踪

```erlang
%% 使用 recon_trace，限制 100 次调用
recon_trace:calls({rabbit_channel, handle_cast, '_'}, 100).

%% 10 秒后自动停止
recon_trace:calls({rabbit_channel, handle_cast, '_'}, {100, 10000}).
```

### 4. 追踪特定参数的调用

```erlang
%% 只追踪第一个参数为 foo 的调用
MS = dbg:fun2ms(fun([foo, _]) -> return_trace() end).
dbg:tpl(mymodule, myfunction, MS).
```

---

## 常见问题

### Q: 为什么 trace_pattern 不生效？

1. 模块未加载：先执行 `l(module)` 或 `code:ensure_loaded(module)`
2. 只使用了 trace 或 trace_pattern 其中一个：两者必须配合使用
3. 使用了 `global` 但调用是模块内部调用：改用 `local`

### Q: 如何获取函数返回值？

使用 `return_trace` MatchSpec，而不是 `return_to` flag：

```erlang
erlang:trace_pattern({M, F, A}, [{'_', [], [{return_trace}]}], [local]).
```

### Q: 为什么看不到追踪消息？

1. 终端作为 tracer 时需要 `flush()`
2. tracer 进程不能追踪自己
3. 检查 trace 和 trace_pattern 是否都设置了

### Q: 如何追踪新创建的进程？

```erlang
%% 使用 set_on_spawn
erlang:trace(all, true, [set_on_spawn, procs, call]).
```

---

## 总结

| 场景 | 推荐工具 |
|------|----------|
| 开发环境快速调试 | `dbg` |
| 生产环境安全追踪 | `recon_trace` |
| 精细控制 | `erlang:trace/3` + `erlang:trace_pattern/3` |
| 消息追踪 | `erlang:trace/3` 的 `send`/`'receive'` |
| 进程追踪 | `erlang:trace/3` 的 `procs` |

掌握 Erlang trace 技术是深入理解 RabbitMQ 等 Erlang 系统的关键。通过合理使用这些工具，可以有效地进行问题排查和性能分析。

---

## 参考链接

- [Erlang trace/3 官方文档](https://www.erlang.org/doc/man/erlang.html#trace-3)
- [Erlang trace_pattern/3 官方文档](https://www.erlang.org/doc/man/erlang.html#trace_pattern-3)
- [Erlang dbg 官方文档](https://www.erlang.org/doc/man/dbg.html)
- [Match Specifications in Erlang](http://erlang.org/documentation/doc-7.0/erts-7.0/doc/html/match_spec.html)
- [Guide to Tracing in Erlang](http://stratus3d.com/blog/2021/08/24/guide-to-tracing-in-erlang/)
- [recon 库文档](https://ferd.github.io/recon/)
