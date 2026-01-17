---
title: RabbitMQ 队列声明流程深度分析
date: 2026-01-17
categories:
  - RabbitMQ
tags:
  - RabbitMQ
  - Erlang
  - 源码分析
  - 队列管理
---

# RabbitMQ 队列声明流程深度分析

## 1. 概述

本文档深入分析 RabbitMQ 队列声明（`rabbit_amqqueue:declare/6`）的完整执行流程，基于以下示例命令：

```erlang
(rabbit@debian-1)1> rabbit_amqqueue:declare(
    rabbit_misc:r(<<"/">>, queue, <<"testqueue2">>),
    false,      % Durable
    false,      % AutoDelete
    [],         % Args
    none,       % Owner
    <<"acting-user">>  % ActingUser
).

%% 返回结果
{new, {amqqueue, {resource,<<"/">>,queue,<<"testqueue2">>},
                 false, false, none, [], <0.862.0>, [], [], [],
                 undefined, undefined, [], [], live, 0, [], <<"/">>,
                 #{user => <<"acting-user">>}}}
```

---

## 2. 流程架构图

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        Queue Declaration Flow                                │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌──────────────┐    ┌─────────────────┐    ┌──────────────────────┐       │
│  │ rabbit_misc  │───▶│ rabbit_amqqueue │───▶│ rabbit_queue_type    │       │
│  │    :r/3      │    │   :declare/6    │    │     :declare/2       │       │
│  └──────────────┘    └─────────────────┘    └──────────────────────┘       │
│         │                    │                        │                     │
│         ▼                    ▼                        ▼                     │
│  ┌──────────────┐    ┌─────────────────┐    ┌──────────────────────┐       │
│  │   #resource  │    │    amqqueue     │    │ rabbit_classic_queue │       │
│  │    record    │    │     :new/9      │    │     :declare/2       │       │
│  └──────────────┘    └─────────────────┘    └──────────────────────┘       │
│                              │                        │                     │
│                              ▼                        ▼                     │
│                      ┌─────────────────┐    ┌──────────────────────┐       │
│                      │ rabbit_policy   │    │rabbit_amqqueue_sup_sup│      │
│                      │     :set/1      │    │:start_queue_process/2│       │
│                      └─────────────────┘    └──────────────────────┘       │
│                              │                        │                     │
│                              ▼                        ▼                     │
│                      ┌─────────────────┐    ┌──────────────────────┐       │
│                      │rabbit_queue_    │    │ rabbit_amqqueue_sup  │       │
│                      │  decorator:set  │    │    :start_link/2     │       │
│                      └─────────────────┘    └──────────────────────┘       │
│                                                       │                     │
│                                                       ▼                     │
│                                             ┌──────────────────────┐       │
│                                             │rabbit_amqqueue_process│      │
│                                             │    :start_link/2      │       │
│                                             └──────────────────────┘       │
│                                                       │                     │
│                                                       ▼                     │
│                                             ┌──────────────────────┐       │
│                                             │    gen_server2:call  │       │
│                                             │    {init, new}       │       │
│                                             └──────────────────────┘       │
│                                                       │                     │
│                                                       ▼                     │
│                                             ┌──────────────────────┐       │
│                                             │ rabbit_db_queue      │       │
│                                             │   :create_or_get/1   │       │
│                                             └──────────────────────┘       │
│                                                       │                     │
│                                                       ▼                     │
│                                             ┌──────────────────────┐       │
│                                             │   Mnesia / Khepri    │       │
│                                             │     持久化存储        │       │
│                                             └──────────────────────┘       │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## 3. 详细流程分析

### 3.1 阶段一：资源名称构建 (`rabbit_misc:r/3`)

#### 源码位置
`deps/rabbit_common/src/rabbit_misc.erl:413-419`

```erlang
r(#resource{virtual_host = VHostPath}, Kind, Name) ->
    #resource{virtual_host = VHostPath, kind = Kind, name = Name};
r(VHostPath, Kind, Name) ->
    #resource{virtual_host = VHostPath, kind = Kind, name = Name}.

r(VHostPath, Kind) ->
    #resource{virtual_host = VHostPath, kind = Kind, name = '_'}.
```

#### 功能说明
- **输入**: `<<"/">>`, `queue`, `<<"testqueue2">>`
- **输出**: `#resource{virtual_host = <<"/">>, kind = queue, name = <<"testqueue2">>}`

#### 数据结构
```erlang
-record(resource, {
    virtual_host :: binary(),  % VHost 名称
    kind :: queue | exchange,  % 资源类型
    name :: binary()           % 资源名称
}).
```

---

### 3.2 阶段二：队列声明入口 (`rabbit_amqqueue:declare/6`)

#### 源码位置
`deps/rabbit/src/rabbit_amqqueue.erl:201-250`

```erlang
declare(QueueName, Durable, AutoDelete, Args, Owner, ActingUser) ->
    declare(QueueName, Durable, AutoDelete, Args, Owner, ActingUser, node()).

declare(QueueName = #resource{virtual_host = VHost}, Durable, AutoDelete, Args,
        Owner, ActingUser, Node) ->
    %% 1. 获取默认队列类型
    DQT = rabbit_vhost:default_queue_type(VHost, rabbit_queue_type:fallback()),
    
    %% 2. 验证声明参数
    ok = check_declare_arguments(QueueName, Args, DQT),
    
    %% 3. 确定队列类型
    Type = get_queue_type(Args, DQT),
    
    %% 4. 检查队列类型是否启用
    case rabbit_queue_type:is_enabled(Type) of
        true ->
            %% 5. 创建 amqqueue 记录
            Q = amqqueue:new(QueueName,
                             none,           % pid (初始为 none)
                             Durable,
                             AutoDelete,
                             Owner,
                             Args,
                             VHost,
                             #{user => ActingUser},
                             Type),
            %% 6. 检查参数组合是否允许
            case is_queue_args_combination_permitted(Q) of
                true ->
                    %% 7. 委托给队列类型模块处理
                    rabbit_queue_type:declare(Q, Node);
                false ->
                    {protocol_error, internal_error, "~ts", [Warning]}
            end;
        false ->
            {protocol_error, internal_error, "...", [...]}
    end.
```

#### 关键步骤详解

##### 3.2.1 获取默认队列类型

```erlang
%% rabbit_vhost.erl
default_queue_type(VHost, DefaultQT) ->
    case lookup(VHost) of
        {ok, VHostRec} ->
            case vhost:get_default_queue_type(VHostRec) of
                undefined -> DefaultQT;
                DQT -> rabbit_queue_type:discover(DQT)
            end;
        {error, _} ->
            DefaultQT
    end.

%% rabbit_queue_type.erl
fallback() -> rabbit_classic_queue.
```

##### 3.2.2 验证声明参数

```erlang
%% rabbit_amqqueue.erl:849-859
check_declare_arguments(QueueName, Args0, DefaultQueueType) ->
    %% 注入默认队列类型
    Args = maybe_inject_default_queue_type_shortcut_into_args(Args0, DefaultQueueType),
    
    %% 检查 x-queue-type 参数
    check_arguments_type_and_value(QueueName, Args, 
        [{<<"x-queue-type">>, fun check_queue_type/2}]),
    
    %% 获取队列类型
    Type = get_queue_type(Args),
    
    %% 获取该类型支持的参数
    QueueTypeArgs = rabbit_queue_type:arguments(queue_arguments, Type),
    
    %% 验证参数类型和值
    Validators = lists:filter(
        fun({Arg, _}) -> lists:member(Arg, QueueTypeArgs) end, 
        declare_args()),
    check_arguments_type_and_value(QueueName, Args, Validators),
    
    %% 检查无效参数
    InvalidArgs = rabbit_queue_type:arguments(queue_arguments) -- QueueTypeArgs,
    check_arguments_key(QueueName, Type, Args, InvalidArgs).
```

##### 3.2.3 确定队列类型

```erlang
%% rabbit_amqqueue.erl:261-273
get_queue_type([], DefaultQueueType) ->
    rabbit_queue_type:discover(DefaultQueueType);
get_queue_type(Args, DefaultQueueType) ->
    case rabbit_misc:table_lookup(Args, <<"x-queue-type">>) of
        undefined ->
            rabbit_queue_type:discover(DefaultQueueType);
        {longstr, undefined} ->
            rabbit_queue_type:discover(DefaultQueueType);
        {longstr, <<"undefined">>} ->
            rabbit_queue_type:discover(DefaultQueueType);
        {_, V} ->
            rabbit_queue_type:discover(V)
    end.
```

**队列类型映射关系**：

| 参数值 | 模块 |
|--------|------|
| `classic` | `rabbit_classic_queue` |
| `quorum` | `rabbit_quorum_queue` |
| `stream` | `rabbit_stream_queue` |

---

### 3.3 阶段三：创建 amqqueue 记录 (`amqqueue:new/9`)

#### 源码位置
`deps/rabbit/src/amqqueue.erl:215-319`

```erlang
new(#resource{kind = queue} = Name,
    Pid,
    Durable,
    AutoDelete,
    Owner,
    Args,
    VHost,
    Options,
    Type)
  when (is_pid(Pid) orelse is_tuple(Pid) orelse Pid =:= none) andalso
       is_boolean(Durable) andalso
       is_boolean(AutoDelete) andalso
       (is_pid(Owner) orelse Owner =:= none) andalso
       is_list(Args) andalso
       (is_binary(VHost) orelse VHost =:= undefined) andalso
       is_map(Options) andalso
       is_atom(Type) ->
    new_with_version(
      ?record_version,  % amqqueue_v2
      Name,
      Pid,
      Durable,
      AutoDelete,
      Owner,
      Args,
      VHost,
      Options,
      Type).

new_with_version(?record_version, Name, Pid, Durable, AutoDelete,
                 Owner, Args, VHost, Options, Type) ->
    #amqqueue{name            = Name,
              durable         = Durable,
              auto_delete     = AutoDelete,
              arguments       = Args,
              exclusive_owner = Owner,
              pid             = Pid,
              vhost           = VHost,
              options         = Options,
              type            = Type}.
```

#### amqqueue 记录结构

```erlang
-record(amqqueue, {
    %% 不可变字段
    name :: rabbit_amqqueue:name() | ets:match_pattern(),
    durable :: boolean() | ets:match_pattern(),
    auto_delete :: boolean() | ets:match_pattern(),
    exclusive_owner = none :: pid() | none | ets:match_pattern(),
    arguments = [] :: rabbit_framing:amqp_table() | ets:match_pattern(),
    
    %% 可变字段
    pid :: pid() | ra_server_id() | none | ets:match_pattern(),
    slave_pids = [],           % 保留字段
    sync_slave_pids = [],      % 保留字段
    recoverable_slaves = [],   % 保留字段
    
    %% 策略相关
    policy :: proplists:proplist() | none | undefined | ets:match_pattern(),
    operator_policy :: proplists:proplist() | none | undefined,
    
    %% 运行时状态
    gm_pids = [] :: [{pid(), pid()}] | none,
    decorators :: [atom()] | none | undefined,
    state = live :: atom(),
    policy_version = 0 :: non_neg_integer(),
    type_state = #{} :: map(),
    vhost :: binary() | undefined,
    options = #{} :: map(),
    type = rabbit_classic_queue :: module()
}).
```

---

### 3.4 阶段四：队列类型分发 (`rabbit_queue_type:declare/2`)

#### 源码位置
`deps/rabbit/src/rabbit_queue_type.erl:389-397`

```erlang
-spec declare(amqqueue:amqqueue(), node() | {'ignore_location', node()}) ->
    {'new' | 'existing' | 'owner_died', amqqueue:amqqueue()} |
    {'absent', amqqueue:amqqueue(), rabbit_amqqueue:absent_reason()} |
    {protocol_error, Type :: atom(), Reason :: string(), Args :: term()} |
    {'error', Type :: atom(), Reason :: string(), Args :: term()} |
    {'error', Err :: term() }.

declare(Q0, Node) ->
    %% 1. 设置策略和装饰器
    Q = rabbit_queue_decorator:set(rabbit_policy:set(Q0)),
    
    %% 2. 获取队列类型模块
    Mod = amqqueue:get_type(Q),
    
    %% 3. 检查队列限制
    case check_queue_limits(Q) of
        ok ->
            %% 4. 调用具体类型的 declare
            Mod:declare(Q, Node);
        Error ->
            Error
    end.
```

#### 策略设置 (`rabbit_policy:set/1`)

```erlang
%% rabbit_policy.erl:94-105
set(Q0) when ?is_amqqueue(Q0) ->
    %% 匹配当前 VHost 的策略
    Policy = match(Q0),
    OpPolicy = match_op(Q0),
    
    %% 设置策略到队列记录
    Q1 = amqqueue:set_policy(Q0, Policy),
    Q2 = amqqueue:set_operator_policy(Q1, OpPolicy),
    Q2.
```

#### 装饰器设置 (`rabbit_queue_decorator:set/1`)

```erlang
%% rabbit_queue_decorator.erl:42-44
set(Q) when ?is_amqqueue(Q) ->
    %% 获取所有活跃的装饰器
    Decorators = [D || D <- list(), D:active_for(Q)],
    amqqueue:set_decorators(Q, Decorators).
```

#### 队列限制检查

```erlang
%% rabbit_queue_type.erl:876-895
check_queue_limits(Q) ->
    maybe
        ok ?= check_vhost_queue_limit(Q),
        ok ?= check_cluster_queue_limit(Q)
    end.

check_vhost_queue_limit(Q) ->
    #resource{name = QueueName} = amqqueue:get_name(Q),
    VHost = amqqueue:get_vhost(Q),
    case rabbit_vhost_limit:is_over_queue_limit(VHost) of
        false -> ok;
        {true, Limit} ->
            queue_limit_error("cannot declare queue '~ts': "
                "queue limit in vhost '~ts' (~tp) is reached",
                [QueueName, VHost, Limit])
    end.
```

---

### 3.5 阶段五：Classic 队列声明 (`rabbit_classic_queue:declare/2`)

#### 源码位置
`deps/rabbit/src/rabbit_classic_queue.erl:120-146`

```erlang
declare(Q, Node) when ?amqqueue_is_classic(Q) ->
    case validate_arguments(amqqueue:get_arguments(Q)) of
        ok -> do_declare(Q, Node);
        Error -> Error
    end.

do_declare(Q, Node) when ?amqqueue_is_classic(Q) ->
    QName = amqqueue:get_name(Q),
    VHost = amqqueue:get_vhost(Q),
    
    %% 1. 确定队列所在节点
    Node1 = case {Node, rabbit_amqqueue:is_exclusive(Q)} of
        {{ignore_location, Node0}, _} ->
            Node0;
        {_, true} ->
            Node;  % 独占队列在当前节点
        _ ->
            %% 使用位置选择策略
            {Node0, _} = rabbit_queue_location:select_leader_and_followers(Q, 1),
            Node0
    end,
    
    %% 2. 检查 VHost 是否存在
    case rabbit_vhost_sup_sup:get_vhost_sup(VHost, Node1) of
        {ok, _} ->
            %% 3. 启动队列进程并等待初始化
            gen_server2:call(
                rabbit_amqqueue_sup_sup:start_queue_process(Node1, Q),
                {init, new}, 
                infinity);
        {error, Error} ->
            {protocol_error, internal_error, 
             "Cannot declare a queue '~ts' on node '~ts': ~255p",
             [rabbit_misc:rs(QName), Node1, Error]}
    end.
```

---

### 3.6 阶段六：启动队列进程

#### 3.6.1 启动队列进程 (`rabbit_amqqueue_sup_sup:start_queue_process/2`)

```erlang
%% rabbit_amqqueue_sup_sup.erl:32-36
start_queue_process(Node, Q) ->
    #resource{virtual_host = VHost} = amqqueue:get_name(Q),
    
    %% 找到 VHost 的队列监督者
    {ok, Sup} = find_for_vhost(VHost, Node),
    
    %% 启动新的队列子进程
    {ok, _SupPid, QPid} = supervisor:start_child(Sup, [Q, declare]),
    QPid.
```

#### 3.6.2 队列监督者初始化 (`rabbit_amqqueue_sup:start_link/2`)

```erlang
%% rabbit_amqqueue_sup.erl:23-37
start_link(Q, _StartMode) ->
    %% 创建 Marker 进程用于区分启动和重启
    Marker = spawn_link(fun() -> receive stop -> ok end end),
    
    %% 定义队列进程的启动规范
    StartMFA = {rabbit_amqqueue_process, start_link, [Q, Marker]},
    ChildSpec = #{id => rabbit_amqqueue,
                  start => StartMFA,
                  restart => transient,
                  significant => true,
                  shutdown => ?CLASSIC_QUEUE_WORKER_WAIT,
                  type => worker,
                  modules => [rabbit_amqqueue_process]},
    
    %% 启动监督者
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    
    %% 启动队列进程
    {ok, QPid} = supervisor:start_child(SupPid, ChildSpec),
    
    %% 清理 Marker
    unlink(Marker),
    Marker ! stop,
    {ok, SupPid, QPid}.
```

---

### 3.7 阶段七：队列进程初始化 (`rabbit_amqqueue_process`)

#### 源码位置
`deps/rabbit/src/rabbit_amqqueue_process.erl:144-239`

```erlang
%% 启动入口
start_link(Q, Marker) ->
    gen_server2:start_link(?MODULE, {Q, Marker}, []).

%% 初始化
init({Q, Marker}) ->
    case is_process_alive(Marker) of
        true  ->
            %% 正常启动
            init(Q);
        false ->
            %% 重启场景
            QueueName = amqqueue:get_name(Q),
            {ok, Q1} = rabbit_amqqueue:lookup(QueueName),
            gen_server2:cast(self(), init),
            init(Q1)
    end;

init(Q) ->
    process_flag(trap_exit, true),
    ?store_proc_name(amqqueue:get_name(Q)),
    {ok, init_state(amqqueue:set_pid(Q, self())), hibernate,
     {backoff, ?HIBERNATE_AFTER_MIN, ?HIBERNATE_AFTER_MIN, ?DESIRED_HIBERNATE},
    ?MODULE}.
```

#### 3.7.1 初始化状态

```erlang
%% rabbit_amqqueue_process.erl:165-180
init_state(Q) ->
    SingleActiveConsumerOn = case rabbit_misc:table_lookup(
            amqqueue:get_arguments(Q), <<"x-single-active-consumer">>) of
        {bool, true} -> true;
        _            -> false
    end,
    State = #q{q                         = Q,
               active_consumer           = none,
               has_had_consumers         = false,
               consumers                 = rabbit_queue_consumers:new(),
               senders                   = pmon:new(delegate),
               msg_id_to_channel         = #{},
               status                    = running,
               args_policy_version       = 0,
               overflow                  = 'drop-head',
               single_active_consumer_on = SingleActiveConsumerOn},
    rabbit_event:init_stats_timer(State, #q.stats_timer).
```

#### 3.7.2 处理 `{init, new}` 调用

```erlang
%% handle_call 会路由到 init_it/3
init_it(Recover, From, State = #q{q = Q})
  when ?amqqueue_exclusive_owner_is(Q, none) ->
    init_it2(Recover, From, State);

init_it2(Recover, From, State = #q{q = Q,
                                   backing_queue = undefined,
                                   backing_queue_state = undefined}) ->
    {Barrier, TermsOrNew} = recovery_status(Recover),
    
    %% 关键：内部声明，写入数据库
    case rabbit_amqqueue:internal_declare(Q, Recover /= new) of
        {Res, Q1} when ?is_amqqueue(Q1) andalso
                       (Res == created orelse Res == existing) ->
            case matches(Recover, Q, Q1) of
                true ->
                    %% 初始化 backing queue
                    BQ = backing_queue_module(),
                    BQS = bq_init(BQ, Q, TermsOrNew),
                    
                    %% 回复调用者
                    send_reply(From, {new, Q}),
                    
                    recovery_barrier(Barrier),
                    
                    %% 处理策略和参数
                    State1 = process_args_policy(
                        State#q{backing_queue = BQ,
                                backing_queue_state = BQS}),
                    
                    %% 通知装饰器
                    notify_decorators(startup, State),
                    
                    %% 发送事件通知
                    rabbit_event:notify(queue_created,
                        queue_created_infos(State1)),
                    
                    %% 发送统计信息
                    rabbit_event:if_enabled(State1, #q.stats_timer,
                        fun() -> emit_stats(State1) end),
                    
                    noreply(State1);
                false ->
                    {stop, normal, {existing, Q1}, State}
            end;
        {error, timeout} ->
            %% 超时处理
            ...;
        Err ->
            {stop, normal, Err, State}
    end.
```

---

### 3.8 阶段八：数据库持久化 (`rabbit_amqqueue:internal_declare/2`)

#### 源码位置
`deps/rabbit/src/rabbit_amqqueue.erl:275-302`

```erlang
-spec internal_declare(Queue, Recover) -> Ret when
      Queue :: amqqueue:amqqueue(),
      Recover :: boolean(),
      Ret :: {created | existing, amqqueue:amqqueue()} |
             queue_absent() |
             rabbit_khepri:timeout_error().

internal_declare(Q, Recover) ->
    do_internal_declare(Q, Recover).

%% 恢复模式
do_internal_declare(Q0, true) ->
    Q = amqqueue:set_state(Q0, live),
    case store_queue(Q) of
        ok -> {created, Q0};
        {error, timeout} = Err -> Err
    end;

%% 新建模式 (我们的示例走这条路径)
do_internal_declare(Q0, false) ->
    %% 1. 设置状态为 live
    %% 2. 应用策略
    Q = rabbit_policy:set(amqqueue:set_state(Q0, live)),
    
    %% 3. 设置装饰器
    Queue = rabbit_queue_decorator:set(Q),
    
    %% 4. 写入数据库
    rabbit_db_queue:create_or_get(Queue).
```

#### 数据库操作 (`rabbit_db_queue:create_or_get/1`)

```erlang
%% rabbit_db_queue.erl:895-930
create_or_get(Q) ->
    rabbit_khepri:handle_fallback(
      #{mnesia => fun() -> create_or_get_in_mnesia(Q) end,
        khepri => fun() -> create_or_get_in_khepri(Q) end
       }).

%% Mnesia 实现
create_or_get_in_mnesia(Q) ->
    DurableQ = amqqueue:reset_decorators(Q),
    QueueName = amqqueue:get_name(Q),
    rabbit_mnesia:execute_mnesia_transaction(
      fun () ->
              case mnesia:wread({?MNESIA_TABLE, QueueName}) of
                  [] ->
                      %% 队列不存在
                      case get_durable_in_mnesia_tx(QueueName) of
                          {error, not_found} ->
                              %% 创建新队列
                              set_in_mnesia_tx(DurableQ, Q),
                              {created, Q};
                          {ok, QRecord} ->
                              %% 存在持久化记录但运行时不存在
                              {absent, QRecord, nodedown}
                      end;
                  [ExistingQ] ->
                      %% 队列已存在
                      {existing, ExistingQ}
              end
      end).

%% Khepri 实现
create_or_get_in_khepri(Q) ->
    QueueName = amqqueue:get_name(Q),
    Path = khepri_queue_path(QueueName),
    case rabbit_khepri:adv_create(Path, Q) of
        {error, {khepri, mismatching_node, #{node_props := #{data := ExistingQ}}}} ->
            {existing, ExistingQ};
        {ok, _} ->
            {created, Q};
        Error ->
            Error
    end.
```

---

## 4. 进程监督树结构

```
rabbit_sup
├── rabbit_vhost_sup_sup
│   └── rabbit_vhost_sup (per VHost)
│       └── rabbit_amqqueue_sup_sup
│           └── rabbit_amqqueue_sup (per Queue)
│               └── rabbit_amqqueue_process (队列工作进程)
```

```
                    rabbit_sup
                        │
                        ▼
              rabbit_vhost_sup_sup
                        │
                        ▼
         ┌──────────────┼──────────────┐
         ▼              ▼              ▼
  rabbit_vhost_sup  rabbit_vhost_sup  ...
   (vhost: /)       (vhost: test)
         │
         ▼
rabbit_amqqueue_sup_sup
         │
         ├──────────────┬──────────────┐
         ▼              ▼              ▼
rabbit_amqqueue_sup  rabbit_amqqueue_sup  ...
  (testqueue1)        (testqueue2)
         │              │
         ▼              ▼
rabbit_amqqueue_     rabbit_amqqueue_
    process              process
   <0.861.0>           <0.862.0>
```

---

## 5. 返回结果解析

```erlang
{new, {amqqueue, {resource,<<"/">>,queue,<<"testqueue2">>},
                 false,        % durable
                 false,        % auto_delete
                 none,         % exclusive_owner
                 [],           % arguments
                 <0.862.0>,    % pid (队列进程)
                 [],           % slave_pids (保留)
                 [],           % sync_slave_pids (保留)
                 [],           % recoverable_slaves (保留)
                 undefined,    % policy
                 undefined,    % operator_policy
                 [],           % gm_pids (保留)
                 [],           % decorators
                 live,         % state
                 0,            % policy_version
                 [],           % type_state
                 <<"/">>,      % vhost
                 #{user => <<"acting-user">>}}}  % options
```

| 字段 | 值 | 说明 |
|------|-----|------|
| 结果类型 | `new` | 新创建的队列 |
| name | `{resource,<<"/">>,queue,<<"testqueue2">>}` | 队列资源标识 |
| durable | `false` | 非持久化队列 |
| auto_delete | `false` | 不自动删除 |
| exclusive_owner | `none` | 非独占队列 |
| arguments | `[]` | 无额外参数 |
| pid | `<0.862.0>` | 队列进程 PID |
| state | `live` | 运行状态 |
| vhost | `<<"/">>` | 所属 VHost |
| options | `#{user => <<"acting-user">>}` | 操作用户 |

---

## 6. 关键数据流图

```
输入参数
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ rabbit_misc:r(<<"/">>, queue, <<"testqueue2">>)             │
│     => #resource{virtual_host = <<"/">>,                    │
│                  kind = queue,                              │
│                  name = <<"testqueue2">>}                   │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ rabbit_amqqueue:declare/6                                   │
│   1. check_declare_arguments/3  ✓                           │
│   2. get_queue_type/2 => rabbit_classic_queue               │
│   3. amqqueue:new/9 => #amqqueue{...}                       │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ rabbit_queue_type:declare/2                                 │
│   1. rabbit_policy:set/1 => 应用策略                         │
│   2. rabbit_queue_decorator:set/1 => 设置装饰器              │
│   3. check_queue_limits/1 ✓                                 │
│   4. Mod:declare/2 => rabbit_classic_queue:declare/2        │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ rabbit_classic_queue:declare/2                              │
│   1. validate_arguments/1 ✓                                 │
│   2. select_leader_and_followers/2 => 选择节点               │
│   3. rabbit_amqqueue_sup_sup:start_queue_process/2          │
│   4. gen_server2:call({init, new}, infinity)                │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ rabbit_amqqueue_process                                     │
│   1. init/1 => 初始化进程状态                                │
│   2. handle_call({init, new}, ...) => init_it2/3            │
│   3. rabbit_amqqueue:internal_declare/2                     │
│   4. rabbit_db_queue:create_or_get/1                        │
│   5. 初始化 backing_queue                                   │
│   6. 发送 queue_created 事件                                 │
│   7. 返回 {new, Q}                                          │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────────────────────────┐
│ rabbit_db_queue:create_or_get/1                             │
│   Mnesia: mnesia:wread + mnesia:write                       │
│   Khepri: rabbit_khepri:adv_create                          │
│   => {created, Q} | {existing, Q}                           │
└─────────────────────────────────────────────────────────────┘
    │
    ▼
返回结果: {new, #amqqueue{...}}
```

---

## 7. 时序图

```
Client          rabbit_amqqueue    rabbit_queue_type   rabbit_classic_queue
   │                   │                   │                    │
   │  declare/6        │                   │                    │
   │──────────────────▶│                   │                    │
   │                   │                   │                    │
   │                   │ check_arguments   │                    │
   │                   │◀─────────────────▶│                    │
   │                   │                   │                    │
   │                   │ amqqueue:new/9    │                    │
   │                   │──────────────────▶│                    │
   │                   │◀──────────────────│                    │
   │                   │                   │                    │
   │                   │ declare/2         │                    │
   │                   │──────────────────▶│                    │
   │                   │                   │ declare/2          │
   │                   │                   │───────────────────▶│
   │                   │                   │                    │
   │                   │                   │                    │

rabbit_classic_queue   rabbit_amqqueue_sup_sup   rabbit_amqqueue_process   rabbit_db_queue
         │                      │                        │                      │
         │ start_queue_process  │                        │                      │
         │─────────────────────▶│                        │                      │
         │                      │ supervisor:start_child │                      │
         │                      │───────────────────────▶│                      │
         │                      │◀───────────────────────│                      │
         │◀─────────────────────│                        │                      │
         │                      │                        │                      │
         │ gen_server2:call     │                        │                      │
         │  {init, new}         │                        │                      │
         │───────────────────────────────────────────────▶│                      │
         │                      │                        │ internal_declare     │
         │                      │                        │─────────────────────▶│
         │                      │                        │ create_or_get        │
         │                      │                        │─────────────────────▶│
         │                      │                        │◀─────────────────────│
         │                      │                        │ {created, Q}         │
         │◀───────────────────────────────────────────────│                      │
         │ {new, Q}             │                        │                      │
```

---

## 8. 队列类型对比

| 特性 | Classic Queue | Quorum Queue | Stream Queue |
|------|---------------|--------------|--------------|
| 模块 | `rabbit_classic_queue` | `rabbit_quorum_queue` | `rabbit_stream_queue` |
| 存储 | `rabbit_variable_queue` | Ra (Raft) | Osiris |
| 持久化 | 可选 | 强制 | 强制 |
| 复制 | 不支持 | 支持 (Raft) | 支持 |
| 声明流程 | `gen_server2:call` | `ra:start_cluster` | `rabbit_stream_coordinator` |

---

## 9. 错误处理

### 9.1 常见错误类型

| 错误 | 原因 | 处理 |
|------|------|------|
| `{existing, Q}` | 队列已存在 | 返回现有队列 |
| `{absent, Q, nodedown}` | 节点宕机 | 返回缺席状态 |
| `{error, timeout}` | 数据库超时 | 返回超时错误 |
| `{protocol_error, ...}` | 参数验证失败 | 返回协议错误 |

### 9.2 队列限制检查

```erlang
%% VHost 队列数量限制
check_vhost_queue_limit(Q) ->
    case rabbit_vhost_limit:is_over_queue_limit(VHost) of
        false -> ok;
        {true, Limit} -> queue_limit_error(...)
    end.

%% 集群队列数量限制
check_cluster_queue_limit(Q) ->
    case rabbit_misc:get_env(rabbit, cluster_queue_limit, infinity) of
        infinity -> ok;
        Limit when Count >= Limit -> queue_limit_error(...);
        _ -> ok
    end.
```

---

## 10. 总结

### 10.1 完整调用链

```
rabbit_misc:r/3
    ↓
rabbit_amqqueue:declare/6
    ↓
rabbit_amqqueue:declare/7
    ↓
    ├── check_declare_arguments/3
    ├── get_queue_type/2
    ├── amqqueue:new/9
    └── rabbit_queue_type:declare/2
            ↓
            ├── rabbit_policy:set/1
            ├── rabbit_queue_decorator:set/1
            ├── check_queue_limits/1
            └── rabbit_classic_queue:declare/2
                    ↓
                    ├── validate_arguments/1
                    ├── rabbit_queue_location:select_leader_and_followers/2
                    ├── rabbit_amqqueue_sup_sup:start_queue_process/2
                    │       ↓
                    │       └── supervisor:start_child/2
                    │               ↓
                    │               └── rabbit_amqqueue_sup:start_link/2
                    │                       ↓
                    │                       └── rabbit_amqqueue_process:start_link/2
                    └── gen_server2:call({init, new}, infinity)
                            ↓
                            └── rabbit_amqqueue_process:init_it2/3
                                    ↓
                                    ├── rabbit_amqqueue:internal_declare/2
                                    │       ↓
                                    │       └── rabbit_db_queue:create_or_get/1
                                    │               ↓
                                    │               └── Mnesia/Khepri 写入
                                    ├── bq_init/3 (backing queue)
                                    ├── notify_decorators/2
                                    └── rabbit_event:notify(queue_created, ...)
```

### 10.2 关键模块职责

| 模块 | 职责 |
|------|------|
| `rabbit_misc` | 通用工具函数，资源名称构建 |
| `rabbit_amqqueue` | 队列管理 API 入口 |
| `rabbit_queue_type` | 队列类型抽象层，分发到具体实现 |
| `rabbit_classic_queue` | Classic 队列类型实现 |
| `rabbit_amqqueue_sup_sup` | 队列监督者的监督者 |
| `rabbit_amqqueue_sup` | 单个队列的监督者 |
| `rabbit_amqqueue_process` | 队列工作进程 (gen_server2) |
| `rabbit_db_queue` | 队列数据库操作 (Mnesia/Khepri) |
| `rabbit_policy` | 策略匹配和应用 |
| `rabbit_queue_decorator` | 队列装饰器管理 |
| `amqqueue` | 队列记录定义和操作 |

---

## 11. 参考源文件

| 文件 | 代码行数 | 说明 |
|------|----------|------|
| `rabbit_amqqueue.erl` | 2142 | 队列管理主模块 |
| `rabbit_queue_type.erl` | ~900 | 队列类型抽象层 |
| `rabbit_classic_queue.erl` | ~600 | Classic 队列实现 |
| `rabbit_amqqueue_process.erl` | ~1500 | 队列工作进程 |
| `rabbit_amqqueue_sup_sup.erl` | 94 | 队列监督者管理 |
| `rabbit_amqqueue_sup.erl` | 49 | 单队列监督者 |
| `rabbit_db_queue.erl` | ~1000 | 数据库操作 |
| `amqqueue.erl` | ~500 | 队列记录定义 |
| `rabbit_policy.erl` | ~500 | 策略管理 |
| `rabbit_misc.erl` | ~1500 | 通用工具 |
