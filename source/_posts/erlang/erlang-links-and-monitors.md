---
title: Erlang 进程链接与监视器
date: 2026-01-16 10:00:00
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

# Links and Monitors for Supervision

When processes depend on each other, we have essentially two choices: to have them live and die together, or to let one process be informed when another one crashes. The "let it fail" philosophy of Erlang, discussed in Chapter 3, leads us to a preference for the latter alternative.

Linked processes form a group that, by default, all crash together. If one of the linked processes exits abnormally, all the other processes in its group get an `EXIT` signal and crash as well. Monitors allow a process to monitor another process without automatically getting coupled into the same group.

<!-- more -->

## Links

Links are bidirectional. If two processes are linked, they are said to belong to the same link set. If one of them terminates abnormally, an exit signal is propagated to all processes that belong to its link set.

For setting up a link between two processes, the following BIFs can be used:

- `link(Pid)` sets up a bidirectional link between the calling process and the process with process identifier `Pid`.

- `spawn_link(Module, Function, Args)` atomically spawns and links to a process. This is equivalent to spawning a process and linking to it, but is done atomically so there is no risk of the spawned process dying before the link is set up.

- `unlink(Pid)` removes any link between the calling process and the process with process identifier `Pid`.

### Example

```erlang
1> Pid = spawn(fun() -> receive X -> X end end).
<0.25.0>
2> link(Pid).
true
3> exit(Pid, kill).
true
** exception exit: killed
```

In this example, linking to the spawned process and then sending a kill signal causes both the spawned process and the shell to crash.

## Trapping Exits

A process can trap exit signals by setting:

```erlang
process_flag(trap_exit, true)
```

When a process is trapping exits, the exit signals are converted into messages of the form `{'EXIT', Pid, Reason}` that can be handled in a normal receive statement.

### Example

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

## Table 2-1. Propagation semantics

| Reason          | Trapping exits (`trap_exit=true`)         | Not trapping exits           |
|-----------------|-------------------------------------------|------------------------------|
| `normal`        | Receives `{'EXIT', Pid, normal}`          | Nothing happens              |
| `kill`          | Terminates with reason `killed`           | Terminates with reason `killed` |
| Other           | Receives `{'EXIT', Pid, Other}`           | Terminates with reason `Other` |

## Monitors

Unlike links, monitors are unidirectional. The monitoring process receives a message `{'DOWN', Reference, process, Pid, Reason}` in its mailbox when the monitored process terminates, but no signal is sent in the opposite direction.

Setting up monitors with the BIF:

```erlang
Reference = monitor(process, Pid)
```

The `Reference` can be used to identify the particular monitor if more than one is set up.

To remove a monitor, use:

```erlang
demonitor(Reference)
```

### Monitors vs Links

| Feature                | Links            | Monitors          |
|-----------------------|------------------|-------------------|
| Directionality        | Bidirectional    | Unidirectional    |
| Default behavior      | Crash together   | Message on exit   |
| Signal propagation    | Both directions  | One direction     |
| Setup function        | `link/1`         | `monitor/2`       |
| Remove function       | `unlink/1`       | `demonitor/1`     |

### Example

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

## Summary

- **Links** create a bidirectional connection between processes. When one dies, all linked processes receive an exit signal.
- **Monitors** create a unidirectional observation. The monitoring process receives a `'DOWN'` message when the monitored process terminates.
- Use `process_flag(trap_exit, true)` to convert exit signals into messages.
- The `kill` reason cannot be trapped and always terminates the process.
