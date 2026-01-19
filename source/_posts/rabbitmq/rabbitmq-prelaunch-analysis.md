---
title: RabbitMQ Prelaunch 模块深度解析
date: 2026-01-19
categories:
  - RabbitMQ
  - RabbitMQ-Deps
tags:
  - RabbitMQ
  - Erlang
  - 源码分析
  - 启动流程
  - Systemd
---

# RabbitMQ Prelaunch 模块深度解析

## 目录

1. [模块概述](#1-模块概述)
2. [模块结构](#2-模块结构)
3. [启动流程详解](#3-启动流程详解)
4. [核心模块解析](#4-核心模块解析)
5. [启动状态机](#5-启动状态机)
6. [Systemd 集成](#6-systemd-集成)
7. [配置加载机制](#7-配置加载机制)
8. [日志系统](#8-日志系统)
9. [分布式系统初始化](#9-分布式系统初始化)
10. [调用关系图](#10-调用关系图)

---

## 1. 模块概述

`rabbitmq_prelaunch` 是 RabbitMQ 启动过程中最先执行的 OTP 应用，负责在主应用 `rabbit` 启动前完成所有预处理工作。

### 1.1 核心职责

| 职责 | 说明 |
|------|------|
| **环境检查** | Erlang/OTP 版本兼容性检查 |
| **配置加载** | 加载和解析配置文件 (Cuttlefish/Erlang Term) |
| **分布式初始化** | 启动 Erlang 分布式、EPMD、节点命名 |
| **日志初始化** | 早期日志系统配置 |
| **状态管理** | 启动状态机和 systemd 通知 |
| **信号处理** | Unix 信号处理器注册 |

### 1.2 设计原则

```
┌─────────────────────────────────────────────────────────────────┐
│                    RabbitMQ 启动时序                            │
├─────────────────────────────────────────────────────────────────┤
│  1. Erlang VM 启动                                              │
│  2. rabbitmq_prelaunch 应用启动 ◄── 本模块                      │
│  3. 其他依赖应用启动                                            │
│  4. rabbit 主应用启动                                           │
│  5. 插件启动                                                    │
└─────────────────────────────────────────────────────────────────┘
```

---

## 2. 模块结构

### 2.1 源文件清单

```
deps/rabbitmq_prelaunch/src/
├── rabbit_prelaunch_app.erl          # OTP application 回调
├── rabbit_prelaunch_sup.erl          # 顶层监督树
├── rabbit_prelaunch.erl              # 核心预启动逻辑
├── rabbit_boot_state.erl             # 启动状态机
├── rabbit_boot_state_sup.erl         # 状态监听器监督树
├── rabbit_boot_state_systemd.erl     # systemd 通知
├── rabbit_boot_state_xterm_titlebar.erl # Xterm 标题栏更新
├── rabbit_prelaunch_conf.erl         # 配置加载
├── rabbit_prelaunch_dist.erl         # Erlang 分布式初始化
├── rabbit_prelaunch_erlang_compat.erl # Erlang 版本检查
├── rabbit_prelaunch_early_logging.erl # 早期日志配置
├── rabbit_prelaunch_sighandler.erl   # Unix 信号处理
├── rabbit_prelaunch_errors.erl       # 错误格式化
├── rabbit_prelaunch_file.erl         # 文件读取工具
├── rabbit_logger_text_fmt.erl        # 文本日志格式化器
├── rabbit_logger_json_fmt.erl        # JSON 日志格式化器
├── rabbit_logger_fmt_helpers.erl     # 日志格式化辅助
└── rabbit_logger_std_h.erl           # 日志处理器
```

### 2.2 监督树结构

```
rabbit_prelaunch_sup (one_for_one)
    │
    ├── rabbit_boot_state_sup (one_for_one)
    │       │
    │       ├── rabbit_boot_state_systemd (gen_server)
    │       │       └── 向 systemd 发送状态通知
    │       │
    │       └── rabbit_boot_state_xterm_titlebar (gen_server)
    │               └── 更新终端标题栏
    │
    └── prelaunch (transient)
            └── rabbit_prelaunch:run_prelaunch_first_phase/0
                    └── 执行预启动配置（不启动进程）
```

---

## 3. 启动流程详解

### 3.1 入口点

```erlang
%% rabbit_prelaunch_app.erl
-module(rabbit_prelaunch_app).
-behaviour(application).

start(_Type, _Args) ->
    rabbit_prelaunch_sup:start_link().
```

### 3.2 监督树初始化

```erlang
%% rabbit_prelaunch_sup.erl
init([]) ->
    BootStateSup = #{id => bootstate,
                     start => {rabbit_boot_state_sup, start_link, []},
                     type => supervisor},
    %% `rabbit_prelaunch` 不启动进程，只配置节点
    Prelaunch = #{id => prelaunch,
                  start => {rabbit_prelaunch, run_prelaunch_first_phase, []},
                  restart => transient},
    Procs = [BootStateSup, Prelaunch],
    {ok, {#{strategy => one_for_one}, Procs}}.
```

### 3.3 预启动第一阶段

```erlang
%% rabbit_prelaunch.erl
run_prelaunch_first_phase() ->
    try
        ok = logger:set_process_metadata(
               #{domain => ?RMQLOG_DOMAIN_PRELAUNCH}),
        do_run()
    catch
        throw:{error, _} = Error ->
            rabbit_prelaunch_errors:log_error(Error),
            set_stop_reason(Error),
            rabbit_boot_state:set(stopped),
            Error;
        Class:Exception:Stacktrace ->
            rabbit_prelaunch_errors:log_exception(Class, Exception, Stacktrace),
            Error = {error, Exception},
            set_stop_reason(Error),
            rabbit_boot_state:set(stopped),
            Error
    end.
```

### 3.4 核心执行流程 `do_run/0`

```erlang
do_run() ->
    %% 1. 标记启动状态为 booting
    clear_stop_reason(),
    rabbit_boot_state:set(booting),

    %% 2. 配置调试追踪（如果请求）
    rabbit_prelaunch_early_logging:enable_quick_dbg(rabbit_env:dbg_config()),

    %% 3. 设置信号处理器
    ok = rabbit_prelaunch_sighandler:setup(),

    %% 4. 确保 Mnesia 未启动（集群一致性检查需要）
    assert_mnesia_is_stopped(),

    %% 5. 获取日志配置前的上下文
    Context0 = rabbit_env:get_context_before_logging_init(),

    %% 6. 设置早期日志
    ok = rabbit_prelaunch_early_logging:setup_early_logging(Context0),

    %% 7. 加载 rabbitmq-env.conf，重新设置日志
    Context1 = rabbit_env:get_context_after_logging_init(Context0),
    ok = rabbit_prelaunch_early_logging:setup_early_logging(Context1),

    %% 8. 完成上下文加载
    Context2 = rabbit_env:get_context_after_reloading_env(Context1),
    store_context(Context2),
    ok = setup_shutdown_func(),

    Context = Context2#{initial_pass => IsInitialPass},

    %% 9. 设置代码路径和应用环境变量
    rabbit_env:context_to_code_path(Context),
    rabbit_env:context_to_app_env_vars(Context),

    %% 10. Erlang/OTP 兼容性检查
    ok = rabbit_prelaunch_erlang_compat:check(Context),

    %% 11. 配置检查和加载
    ok = rabbit_prelaunch_conf:setup(Context),

    %% 12. Erlang 分布式检查和启动
    ok = rabbit_prelaunch_dist:setup(Context),

    %% 13. 写入 PID 文件
    _ = write_pid_file(Context),

    %% 14. 垃圾回收
    _ = erlang:garbage_collect(),

    ignore.
```

### 3.5 执行流程图

```
┌─────────────────────────────────────────────────────────────────┐
│                run_prelaunch_first_phase()                      │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 1. rabbit_boot_state:set(booting)                               │
│    └── 通知 systemd: "Startup in progress"                      │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 2. rabbit_prelaunch_sighandler:setup()                          │
│    └── 注册 SIGHUP, SIGTSTP 信号处理器                          │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 3. assert_mnesia_is_stopped()                                   │
│    └── 确保 Mnesia 未运行（集群一致性检查需要）                  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 4. rabbit_prelaunch_early_logging:setup_early_logging()         │
│    └── 配置早期日志系统                                         │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 5. rabbit_prelaunch_erlang_compat:check()                       │
│    └── 检查 Erlang/OTP >= 26.0                                  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 6. rabbit_prelaunch_conf:setup()                                │
│    ├── 设置默认配置 (Ra, Aten, Mnesia 等)                       │
│    ├── 查找配置文件 (.conf/.config)                             │
│    ├── 加载 Cuttlefish schema                                   │
│    ├── 解析配置文件                                             │
│    └── 解密加密配置值                                           │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 7. rabbit_prelaunch_dist:setup()                                │
│    ├── 确保 EPMD 运行                                           │
│    ├── 检查端口范围和可用性                                     │
│    ├── 检查节点名是否重复                                       │
│    ├── 启动 net_kernel                                          │
│    └── 设置凭据混淆密钥                                         │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 8. write_pid_file()                                             │
│    └── 写入 PID 文件到指定路径                                  │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│ 返回 ignore (transient worker 正常退出)                         │
└─────────────────────────────────────────────────────────────────┘
```

---

## 4. 核心模块解析

### 4.1 rabbit_prelaunch.erl

**职责**：协调整个预启动流程

**关键函数**：

```erlang
%% 存储和获取上下文（使用 persistent_term 高效存储）
store_context(Context) when is_map(Context) ->
    persistent_term:put(?PT_KEY_CONTEXT, Context).

get_context() ->
    case persistent_term:get(?PT_KEY_CONTEXT, undefined) of
        undefined -> undefined;
        Context   -> Context#{initial_pass => is_initial_pass()}
    end.

%% 停止原因管理
set_stop_reason(Reason) ->
    case get_stop_reason() of
        undefined ->
            persistent_term:put(?PT_KEY_STOP_REASON, Reason);
        _ ->
            ok
    end.

%% 初始化 pass 标记
is_initial_pass() ->
    not persistent_term:get(?PT_KEY_INITIAL_PASS, false).

initial_pass_finished() ->
    persistent_term:put(?PT_KEY_INITIAL_PASS, true).

%% 关机函数链
setup_shutdown_func() ->
    ExistingShutdownFunc = application:get_env(kernel, shutdown_func),
    case ExistingShutdownFunc of
        {ok, {ExistingMod, ExistingFunc}} ->
            %% 链式调用：先执行我们的，再执行原有的
            ok = persistent_term:put(?PT_KEY_SHUTDOWN_FUNC, ExistingShutdownFunc),
            ok = record_kernel_shutdown_func(?MODULE, shutdown_func);
        _ ->
            ok = record_kernel_shutdown_func(?MODULE, shutdown_func)
    end.

shutdown_func(Reason) ->
    Context = get_context(),
    remove_pid_file(Context),  %% 清理 PID 文件
    %% 调用链式的关机函数
    case persistent_term:get(?PT_KEY_SHUTDOWN_FUNC, undefined) of
        {ChainedMod, ChainedFunc} -> ChainedMod:ChainedFunc(Reason);
        _                         -> ok
    end.
```

### 4.2 rabbit_prelaunch_erlang_compat.erl

**职责**：Erlang/OTP 版本兼容性检查

```erlang
%% 最低版本要求
-define(OTP_MINIMUM, "26.0").
-define(ERTS_MINIMUM, "14.0").

check(_Context) ->
    ERTSVer = erlang:system_info(version),
    OTPRel = rabbit_misc:otp_release(),
    
    case rabbit_misc:version_compare(?ERTS_MINIMUM, ERTSVer, lte) of
        true when ?ERTS_MINIMUM =/= ERTSVer ->
            ok;  %% 版本满足要求
        true when ?ERTS_MINIMUM =:= ERTSVer andalso ?OTP_MINIMUM =< OTPRel ->
            ok;  %% 同 ERTS 版本但 OTP release 更新
        _ ->
            %% 版本过旧，抛出错误
            throw({error, {erlang_version_too_old, Msg}})
    end.
```

### 4.3 rabbit_prelaunch_sighandler.erl

**职责**：Unix 信号处理

```erlang
%% 信号处理配置
-define(SIGNALS_HANDLED_BY_US,
        #{
          sighup => ignore,   %% 忽略 SIGHUP（暂不支持配置重载）
          sigtstp => ignore   %% 忽略 SIGTSTP (Ctrl+Z)
         }).

%% 由 Erlang 处理的信号
-define(SIGNAL_HANDLED_BY_ERLANG(Signal),
        Signal =:= sigusr1 orelse
        Signal =:= sigquit orelse
        Signal =:= sigterm).

setup() ->
    case os:type() of
        {unix, _} ->
            case whereis(erl_signal_server) of
                undefined -> ok;
                _ ->
                    gen_event:add_handler(erl_signal_server, ?MODULE, [])
            end;
        _ ->
            ok  %% Windows 不处理信号
    end.

init(_Args) ->
    maps:fold(
      fun
          (Signal, _, Ret) when ?SIGNAL_HANDLED_BY_ERLANG(Signal) -> Ret;
          (Signal, default, ok) -> os:set_signal(Signal, default);
          (Signal, ignore, ok)  -> os:set_signal(Signal, ignore);
          (Signal, _, ok)       -> os:set_signal(Signal, handle)
      end, ok, ?SIGNALS_HANDLED_BY_US),
    {ok, #{}}.
```

---

## 5. 启动状态机

### 5.1 状态定义

```erlang
%% rabbit_boot_state.erl
-type boot_state() :: stopped |      %% 0 - 已停止
                      booting |      %% 1 - 启动中
                      core_started | %% 2 - 核心已启动
                      ready |        %% 3 - 就绪
                      stopping.      %% 4 - 停止中
```

### 5.2 状态转换图

```
                    ┌─────────┐
                    │ stopped │ ◄────────────────────────────┐
                    │  (0)    │                              │
                    └────┬────┘                              │
                         │ set(booting)                      │
                         ▼                                   │
                    ┌─────────┐                              │
                    │ booting │                              │
                    │  (1)    │                              │
                    └────┬────┘                              │
                         │ set(core_started)                 │
                         ▼                                   │
               ┌──────────────────┐                          │
               │  core_started    │                          │
               │      (2)         │                          │
               └────────┬─────────┘                          │
                        │ set(ready)                         │
                        ▼                                    │
                    ┌─────────┐                              │
                    │  ready  │ ─────set(stopping)───►┌──────┴─────┐
                    │  (3)    │                       │  stopping  │
                    └─────────┘                       │    (4)     │
                                                      └────────────┘
```

### 5.3 状态设置与通知

```erlang
%% rabbit_boot_state.erl
-spec set(boot_state()) -> ok.
set(BootState) ->
    ?LOG_DEBUG("Change boot state to `~ts`", [BootState]),
    case BootState of
        stopped -> persistent_term:erase(?PT_KEY_BOOT_STATE);
        _       -> persistent_term:put(?PT_KEY_BOOT_STATE, BootState)
    end,
    %% 通知所有监听器
    rabbit_boot_state_sup:notify_boot_state_listeners(BootState).

%% rabbit_boot_state_sup.erl
notify_boot_state_listeners(BootState) ->
    lists:foreach(
      fun
          ({_, Child, _, _}) when is_pid(Child) ->
              gen_server:cast(Child, {notify_boot_state, BootState});
          (_) ->
              ok
      end,
      supervisor:which_children(?MODULE)).
```

### 5.4 状态等待

```erlang
%% 等待达到指定状态
-spec wait_for(boot_state(), timeout()) -> ok | {error, timeout}.
wait_for(BootState, infinity) ->
    case has_reached(BootState) of
        true  -> ok;
        false -> Wait = 200,
                 timer:sleep(Wait),
                 wait_for(BootState, infinity)
    end;
wait_for(BootState, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    case has_reached(BootState) of
        true  -> ok;
        false -> Wait = 200,
                 timer:sleep(Wait),
                 wait_for(BootState, Timeout - Wait)
    end.

%% 判断是否已达到目标状态
has_reached(CurrentBootState, TargetBootState) ->
    boot_state_idx(TargetBootState) =< boot_state_idx(CurrentBootState).
```

---

## 6. Systemd 集成

### 6.1 systemd 通知模块

```erlang
%% rabbit_boot_state_systemd.erl
-module(rabbit_boot_state_systemd).
-behaviour(gen_server).

init([]) ->
    %% 启动 systemd 应用
    {ok, _} = application:ensure_all_started(systemd),
    {ok, #{}}.

handle_cast({notify_boot_state, BootState}, State) ->
    _ = notify_boot_state(BootState),
    {noreply, State}.

%% 通知逻辑
notify_boot_state(ready = BootState) ->
    Status = boot_state_to_desc(BootState),
    %% ready 状态发送 READY=1 通知
    systemd:notify([BootState, {status, Status}]);
notify_boot_state(BootState) ->
    Status = boot_state_to_desc(BootState),
    %% 其他状态只发送状态描述
    systemd:notify({status, Status}).

%% 状态描述映射
boot_state_to_desc(stopped)      -> "Standing by";
boot_state_to_desc(booting)      -> "Startup in progress";
boot_state_to_desc(core_started) -> "Startup in progress (core ready, starting plugins)";
boot_state_to_desc(ready)        -> "";  %% systemd 使用默认
boot_state_to_desc(stopping)     -> "".
```

### 6.2 状态通知时序

```
┌─────────────────────────────────────────────────────────────────┐
│ RabbitMQ 启动                                                   │
│                                                                 │
│ 1. booting:                                                     │
│    └── systemd:notify({status, "Startup in progress"})          │
│        └── systemctl status 显示: Status: "Startup in progress" │
│                                                                 │
│ 2. core_started:                                                │
│    └── systemd:notify({status, "...(core ready, starting...)"}) │
│                                                                 │
│ 3. ready:                                                       │
│    └── systemd:notify([ready, {status, ""}])                    │
│        └── 发送 READY=1，systemd 标记服务为 active              │
│        └── systemctl status 显示: Status: "Initialized"         │
└─────────────────────────────────────────────────────────────────┘
```

### 6.3 Xterm 标题栏更新

```erlang
%% rabbit_boot_state_xterm_titlebar.erl
%% 在 Unix 系统的交互式终端中更新标题栏

init([]) ->
    RunsOnUnix = case os:type() of
                     {unix, _} -> true;
                     _         -> false
                 end,
    AcceptsInput = case init:get_argument(noinput) of
                       {ok, _} -> false;
                       error   -> true
                   end,
    case RunsOnUnix andalso AcceptsInput of
        true ->
            RawStdio = erlang:open_port({fd, 0, 1}, [out]),
            {ok, #?MODULE{raw_stdio_port = RawStdio}};
        false ->
            ignore
    end.

set_xterm_titlebar(#?MODULE{raw_stdio_port = RawStdio}, BootState) ->
    Title = format_title(BootState),
    Binary = unicode:characters_to_binary(Title),
    %% Xterm OSC (Operating System Command) 序列
    %% \033]2; ... \007 设置窗口标题
    erlang:port_command(RawStdio, ["\033]2;", Binary, "\007"]).

format_title(BootState) ->
    {ok, Vsn} = application:get_key(rabbitmq_prelaunch, vsn),
    BootStateSuffix = case BootState of
                          ready -> "";
                          _     -> io_lib:format(": ~ts", [BootState])
                      end,
    case node() of
        nonode@nohost ->
            rabbit_misc:format("RabbitMQ ~ts~ts", [Vsn, BootStateSuffix]);
        Node ->
            rabbit_misc:format("~ts — RabbitMQ ~ts~ts", [Node, Vsn, BootStateSuffix])
    end.
```

---

## 7. 配置加载机制

### 7.1 配置文件类型

```erlang
%% rabbit_prelaunch_conf.erl

%% 支持两种配置格式
%% 1. Cuttlefish 格式 (.conf) - 推荐
%% 2. Erlang Term 格式 (.config) - 传统

find_actual_main_config_file(#{main_config_file := File}) ->
    case filelib:is_regular(File) of
        true ->
            Format = case filename:extension(File) of
                ".conf"   -> cuttlefish;  %% 新格式
                ".config" -> erlang;       %% 旧格式
                _         -> determine_config_format(File)
            end,
            {File, Format};
        false ->
            %% 尝试添加扩展名
            OldFormatFile = File ++ ".config",
            NewFormatFile = File ++ ".conf",
            ...
    end.
```

### 7.2 默认配置设置

```erlang
set_default_config() ->
    Config = [
              {ra,           %% Raft 算法库配置
               [{wal_max_size_bytes, 536870912},  %% 512MB
                {wal_max_batch_size, 4096}]},
              {aten,         %% 心跳检测库配置
               [{poll_interval, 5000}]},  %% 5秒心跳
              {syslog,
               [{app_name, "rabbitmq-server"}]},
              {sysmon_handler,  %% 系统监控
               [{process_limit, 100},
                {port_limit, 100},
                {gc_ms_limit, 0},
                {schedule_ms_limit, 0},
                {heap_word_limit, 0},
                {busy_port, false},
                {busy_dist_port, true}]},
              {mnesia,
               [{dump_log_write_threshold, 5000},
                {dump_log_time_threshold, 90000}]}
             ],
    apply_erlang_term_based_config(Config).
```

### 7.3 Cuttlefish Schema 加载

```erlang
generate_config_from_cuttlefish_files(Context, ConfigFiles, AdvancedConfigFile) ->
    %% 1. 查找所有 schema 文件
    SchemaFiles = find_cuttlefish_schemas(Context),
    Schema = cuttlefish_schema:files(SchemaFiles),

    %% 2. 解析配置文件
    case cuttlefish_conf:files(ConfigFiles) of
        {errorlist, Errors} ->
            throw({error, failed_to_parse_configuration_file});
        Config0 ->
            %% 3. 生成最终配置
            Config = cuttlefish_generator:map(Schema, Config0),
            
            %% 4. 应用高级配置覆盖
            override_with_advanced_config(Config, AdvancedConfigFile)
    end.

%% 查找 priv/schema/ 目录下的 schema 文件
find_cuttlefish_schemas(Context) ->
    Apps = list_apps(Context),
    [filename:join([PrivDir, "schema", File]) 
     || App <- Apps, 
        PrivDir <- [code:priv_dir(App)],
        File <- list_schema_files(PrivDir)].
```

### 7.4 配置解密

```erlang
%% 支持加密配置值
decrypt({encrypted, _} = EncValue,
        {Cipher, Hash, Iterations, PassPhrase} = Algo) ->
    {rabbit_pbe:decrypt_term(Cipher, Hash, Iterations, PassPhrase, EncValue),
     Algo};

%% 获取解密密码
get_passphrase(ConfigEntryDecoder) ->
    case proplists:get_value(passphrase, ConfigEntryDecoder) of
        prompt ->
            %% 交互式输入
            IoDevice = get_input_iodevice(),
            ok = io:setopts(IoDevice, [{echo, false}]),
            PP = io:get_line(IoDevice, "Passphrase: "),
            ok = io:setopts(IoDevice, [{echo, true}]),
            PP;
        {file, Filename} ->
            %% 从文件读取
            {ok, File} = rabbit_misc:raw_read_file(Filename),
            [PP|_] = binary:split(File, [<<"\r\n">>, <<"\n">>]),
            PP;
        PP ->
            %% 直接使用
            PP
    end.
```

---

## 8. 日志系统

### 8.1 早期日志初始化

```erlang
%% rabbit_prelaunch_early_logging.erl

setup_early_logging(#{log_levels := undefined} = Context) ->
    setup_early_logging(Context#{log_levels => #{"prelaunch" => notice}});
setup_early_logging(Context) ->
    case is_configured() of
        true  -> ok;
        false -> do_setup_early_logging(Context)
    end.

do_setup_early_logging(#{log_levels := LogLevels} = Context) ->
    add_rmqlog_filter(LogLevels),
    ok = logger:update_handler_config(default, main_handler_config(Context)).

add_rmqlog_filter(LogLevels) ->
    %% 添加主过滤器
    ok = logger:add_primary_filter(
          progress_reports, {fun logger_filters:progress/2, stop}),
    ok = logger:add_primary_filter(
          discarded_messages, {fun filter_discarded_message/2, stop}),
    
    %% 添加处理器过滤器
    ok = logger:add_handler_filter(
           default, ?FILTER_NAME, {fun filter_log_event/2, FilterConfig}),
    ok = logger:set_primary_config(level, all).
```

### 8.2 日志格式化器

**文本格式化器** `rabbit_logger_text_fmt.erl`:
```erlang
format(#{msg := Msg, meta := Meta} = LogEvent, Config) ->
    Prefix = format_prefix(LogEvent, Config),
    Color = pick_color(LogEvent, Config),
    FormattedMsg = rabbit_logger_fmt_helpers:format_msg(Msg, Meta, Config),
    prepend_prefix_to_msg_and_add_color(Prefix, Color, FormattedMsg, LogEvent, Config).

%% 默认前缀格式: "时间 [级别] PID "
format_prefix(LogEvent, Config) ->
    format_prefix([time, " [", level, "] ", pid, " "], LogEvent, Config, []).

%% 颜色支持
level_to_color(debug, _)     -> "\033[38;5;246m";   %% 灰色
level_to_color(info, _)      -> "";                  %% 默认
level_to_color(notice, _)    -> "\033[38;5;87m";    %% 青色
level_to_color(warning, _)   -> "\033[38;5;214m";   %% 橙色
level_to_color(error, _)     -> "\033[38;5;160m";   %% 红色
level_to_color(critical, _)  -> "\033[1;37m\033[48;5;20m";   %% 白字蓝底
level_to_color(alert, _)     -> "\033[1;37m\033[48;5;93m";   %% 白字紫底
level_to_color(emergency, _) -> "\033[1;37m\033[48;5;196m".  %% 白字红底
```

**JSON 格式化器** `rabbit_logger_json_fmt.erl`:
```erlang
format(#{msg := Msg, level := Level, meta := Meta}, Config) ->
    FormattedLevel = rabbit_logger_fmt_helpers:format_level(Level, Config),
    FormattedMeta = format_meta(Meta, Config),
    FormattedMsg = rabbit_logger_fmt_helpers:format_msg(Msg, Meta, Config),
    InitialDoc = FormattedMeta#{level => FormattedLevel, msg => FormattedMsg},
    DocAfterMapping = apply_mapping_and_ordering(InitialDoc, Config),
    Json = rabbit_json:encode(DocAfterMapping),
    [Json, $\n].
```

---

## 9. 分布式系统初始化

### 9.1 Erlang 分布式设置

```erlang
%% rabbit_prelaunch_dist.erl

setup(#{nodename := Node, nodename_type := NameType} = Context) ->
    case node() of
        nonode@nohost ->
            %% 1. 确保 EPMD 运行
            ok = rabbit_nodes_common:ensure_epmd(),
            
            %% 2. 检查端口范围
            ok = dist_port_range_check(Context),
            
            %% 3. 检查端口是否可用
            ok = dist_port_use_check(Context),
            
            %% 4. 检查节点名是否重复
            ok = duplicate_node_check(Context),
            
            %% 5. 启动分布式
            ok = do_setup(Context);
        Node ->
            ok;  %% 已经是正确的节点名
        Unexpected ->
            throw({error, {erlang_dist_running_with_unexpected_nodename,
                           Unexpected, Node}})
    end,
    %% 6. 设置凭据混淆密钥
    ok = set_credentials_obfuscation_secret().

do_setup(#{nodename := Node, nodename_type := NameType} = Config) ->
    %% 启动 net_kernel
    case application:get_env(kernel, net_ticktime) of
        {ok, Ticktime} when is_integer(Ticktime), Ticktime >= 1 ->
            MTTI = Ticktime * 1000 div 4,
            {ok, _} = net_kernel:start([Node, NameType, MTTI]);
        _ ->
            {ok, _} = net_kernel:start([Node, NameType])
    end,
    
    %% 处理环境变量中的 Cookie 覆盖
    case maps:get(erlang_cookie, Origins, default) of
        environment ->
            Cookie = maps:get(erlang_cookie, Config),
            true = erlang:set_cookie(node(), Cookie);
        _ ->
            ok
    end.
```

### 9.2 节点重复检查

```erlang
duplicate_node_check(#{split_nodename := {NodeName, NodeHost}}) ->
    %% 临时启动一个分布式节点来检查
    PrelaunchName = rabbit_nodes_common:make(
                      {NodeName ++ "_prelaunch_" ++ os:getpid(), "localhost"}),
    {ok, _} = net_kernel:start([PrelaunchName, shortnames]),
    
    case rabbit_nodes_common:names(NodeHost) of
        {ok, NamePorts} ->
            case proplists:is_defined(NodeName, NamePorts) of
                true ->
                    throw({error, {duplicate_node_name, NodeName, NodeHost}});
                false ->
                    ok = net_kernel:stop(),
                    ok
            end;
        {error, EpmdReason} ->
            throw({error, {epmd_error, NodeHost, EpmdReason}})
    end.
```

### 9.3 端口检查

```erlang
dist_port_use_check_ipv4(NodeHost, Port) ->
    case gen_tcp:listen(Port, [inet, {reuseaddr, true}]) of
        {ok, Sock} -> gen_tcp:close(Sock);
        {error, einval} -> dist_port_use_check_ipv6(NodeHost, Port);
        {error, _} -> dist_port_use_check_fail(Port, NodeHost)
    end.

dist_port_use_check_ipv6(NodeHost, Port) ->
    case gen_tcp:listen(Port, [inet6, {reuseaddr, true}]) of
        {ok, Sock} -> gen_tcp:close(Sock);
        {error, _} -> dist_port_use_check_fail(Port, NodeHost)
    end.
```

---

## 10. 调用关系图

### 10.1 模块依赖关系

```
┌─────────────────────────────────────────────────────────────────┐
│                    rabbit_prelaunch_app                         │
│                           │                                     │
│                           ▼                                     │
│                    rabbit_prelaunch_sup                         │
│                    /                \                           │
│                   ▼                  ▼                          │
│     rabbit_boot_state_sup      rabbit_prelaunch                 │
│           /        \                 │                          │
│          ▼          ▼                │                          │
│   systemd    xterm_titlebar          │                          │
│                                      │                          │
│           ┌──────────────────────────┼──────────────────────┐   │
│           │                          │                      │   │
│           ▼                          ▼                      ▼   │
│   rabbit_prelaunch_     rabbit_prelaunch_    rabbit_prelaunch_  │
│   erlang_compat         conf                 dist               │
│           │                          │                      │   │
│           │                          │                      │   │
│           ▼                          ▼                      ▼   │
│   rabbit_prelaunch_     rabbit_prelaunch_    rabbit_nodes_      │
│   errors                file                 common             │
│           │                          │                          │
│           ▼                          ▼                          │
│   rabbit_prelaunch_     rabbit_logger_       credentials_       │
│   sighandler            *_fmt                obfuscation        │
└─────────────────────────────────────────────────────────────────┘
```

### 10.2 启动时序图

```
     Time
       │
       │  application:start(rabbitmq_prelaunch)
       │  ┌─────────────────────────────────────┐
       │  │ rabbit_prelaunch_app:start()        │
       │  └─────────────────────────────────────┘
       │                    │
       ▼                    ▼
       │  ┌─────────────────────────────────────┐
       │  │ rabbit_prelaunch_sup:start_link()   │
       │  └─────────────────────────────────────┘
       │                    │
       │           ┌───────┴────────┐
       │           ▼                ▼
       │  ┌────────────────┐ ┌──────────────────────────┐
       │  │ boot_state_sup │ │ prelaunch (transient)    │
       │  └────────────────┘ └──────────────────────────┘
       │         │                      │
       │    ┌────┴────┐                 │
       │    ▼         ▼                 ▼
       │ systemd  xterm      run_prelaunch_first_phase()
       │    │                          │
       ▼    │                          │
       │    │  ◄─ boot_state:set(booting) ──────┤
       │    │                          │
       │    │                          ▼
       │    │                 erlang_compat:check()
       │    │                          │
       │    │                          ▼
       │    │                 conf:setup()
       │    │                          │
       │    │                          ▼
       │    │                 dist:setup()
       │    │                          │
       │    │                          ▼
       │    │                 write_pid_file()
       │    │                          │
       │    │                          ▼
       │    │                 return ignore
       │    │                          │
       ▼    │                          │
       │    │  后续由 rabbit 应用设置:  │
       │    │  ◄─ boot_state:set(core_started) ─┤
       │    │  ◄─ boot_state:set(ready) ────────┤
       │    │                          │
       ▼    ▼                          ▼
```

---

## 11. 错误处理

### 11.1 错误格式化

```erlang
%% rabbit_prelaunch_errors.erl

format_error({error, {duplicate_node_name, NodeName, NodeHost}}) ->
    rabbit_misc:format(
      "ERROR: node with name ~tp is already running on host ~tp",
      [NodeName, NodeHost]);

format_error({error, {epmd_error, NodeHost, EpmdReason}}) ->
    rabbit_misc:format(
      "ERROR: epmd error for host ~ts: ~ts",
      [NodeHost, rabbit_misc:format_inet_error(EpmdReason)]);

format_error({error, {dist_port_already_used, Port, Name, Host}}) ->
    rabbit_misc:format(
      "ERROR: could not bind to distribution port ~b, it is in use by "
      "another node: ~ts@~ts", [Port, Name, Host]);

format_error({error, {erlang_version_too_old, Msg}}) ->
    Msg;

%% 启动失败的统一输出格式
log_message(Message) ->
    Lines = string:split(
              "\nBOOT FAILED\n===========\n" ++ Message ++ "\n",
              [$\n], all),
    ?LOG_ERROR("~ts", [string:join(Lines, "\n")]),
    lists:foreach(
      fun(Line) -> io:format(standard_error, "~ts~n", [Line]) end,
      Lines).
```

---

## 12. 总结

### 12.1 关键设计模式

| 模式 | 应用场景 |
|------|---------|
| **Supervisor** | 监督树管理进程生命周期 |
| **gen_server** | systemd 和 xterm 状态监听器 |
| **gen_event** | 信号处理器 |
| **persistent_term** | 高效存储全局配置和状态 |
| **Transient Worker** | prelaunch 执行后退出 |

### 12.2 核心流程总结

1. **启动监督树** → 启动状态监听器
2. **设置启动状态** → 通知 systemd "booting"
3. **检查环境** → Erlang 版本、Mnesia 状态
4. **加载配置** → Cuttlefish/Erlang Term 格式
5. **启动分布式** → EPMD、net_kernel
6. **写入 PID 文件** → 进程管理
7. **完成预启动** → 返回 ignore，worker 正常退出

### 12.3 与 RabbitMQ 主应用的关系

```
rabbitmq_prelaunch 完成          rabbit 应用启动
        │                              │
        │                              ▼
        │                    boot_state:set(core_started)
        │                              │
        │                              ▼
        │                    加载插件、启动服务
        │                              │
        │                              ▼
        │                    boot_state:set(ready)
        │                              │
        │                              ▼
        └───────────────────► systemd 收到 READY=1
                               服务标记为 active
```

---

## 参考资料

- [RabbitMQ 源码](https://github.com/rabbitmq/rabbitmq-server)
- [Cuttlefish](https://github.com/basho/cuttlefish) - 配置管理库
- [systemd Erlang 库](https://github.com/hauleth/systemd)
- [Erlang OTP Supervisor](https://www.erlang.org/doc/design_principles/sup_princ.html)
