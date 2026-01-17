---
title: RabbitMQ 依赖分析：base64url 模块深度解析
date: 2026-01-17
categories:
  - RabbitMQ
  - RabbitMQ Deps
tags:
  - RabbitMQ
  - Erlang
  - 依赖分析
  - Base64
  - 源码分析
---

# Base64url 模块深度解析

## 1. 模块概述

`base64url` 是 RabbitMQ 依赖的一个 URL 安全的 Base64 编解码器，遵循 [RFC 4648](http://tools.ietf.org/html/rfc4648) 标准。

### 1.1 为什么需要 URL 安全的 Base64？

标准 Base64 编码会产生以下在 URL 中有特殊含义的字符：

| 字符 | 标准 Base64 含义 | URL 中的含义 | URL 安全替代 |
|------|-----------------|-------------|-------------|
| `+` | 第62个字符 | 空格 (form encoding) | `-` |
| `/` | 第63个字符 | 路径分隔符 | `_` |
| `=` | 填充字符 | 参数分隔符 | 省略 |

### 1.2 对比示例

```erlang
%% 标准 Base64 (URL 不安全)
base64:encode(<<255,127,254,252>>).
%% 结果: <<"/3/+/A==">>
%%       包含 / + = 三种 URL 危险字符

%% URL 安全 Base64
base64url:encode(<<255,127,254,252>>).
%% 结果: <<"_3_-_A">>
%%       / → _,  + → -,  = 被移除
```

---

## 2. 源码逐行解析

### 2.1 模块声明部分

```erlang
%%
%% @doc URL safe base64-compatible codec.
%%
%% Based heavily on the code extracted from:
%%   https://github.com/basho/riak_control/blob/master/src/base64url.erl and
%%   https://github.com/mochi/mochiweb/blob/master/src/mochiweb_base64url.erl.
%%

-module(base64url).
-author('Vladimir Dronnikov <dronnikov@gmail.com>').

-export([
    decode/1,
    encode/1,
    encode_mime/1
  ]).
```

**解析**：
- `@doc` - EDoc 文档注释
- `-module(base64url)` - 模块名定义
- `-author(...)` - 作者声明
- `-export([...])` - 导出 3 个公共函数

---

### 2.2 encode/1 函数

```erlang
-spec encode(
    binary() | iolist()
  ) -> binary().

encode(Bin) when is_binary(Bin) ->
  << << (urlencode_digit(D)) >> || <<D>> <= base64:encode(Bin), D =/= $= >>;
encode(L) when is_list(L) ->
  encode(iolist_to_binary(L)).
```

**逐行解析**：

#### 类型规范
```erlang
-spec encode(binary() | iolist()) -> binary().
```
- 输入：二进制或 iolist（嵌套列表/二进制）
- 输出：二进制

#### 二进制处理分支
```erlang
encode(Bin) when is_binary(Bin) ->
  << << (urlencode_digit(D)) >> || <<D>> <= base64:encode(Bin), D =/= $= >>;
```

这是一个**二进制推导式 (Binary Comprehension)**，等价于：

```erlang
encode(Bin) ->
    Encoded = base64:encode(Bin),        %% 步骤1: 标准 Base64 编码
    FilteredAndMapped = lists:filtermap(
        fun(D) ->
            if D =/= $= ->               %% 步骤2: 过滤掉填充字符 '='
                {true, urlencode_digit(D)};  %% 步骤3: 字符替换
               true -> false
            end
        end,
        binary_to_list(Encoded)
    ),
    list_to_binary(FilteredAndMapped).
```

**语法拆解**：
```
<< << (urlencode_digit(D)) >> || <<D>> <= base64:encode(Bin), D =/= $= >>
   ├─────────────────────┤    ├─────┤   ├────────────────┤  ├────────┤
         输出表达式        生成器      生成源               过滤条件
```

- `<<D>> <= base64:encode(Bin)` - 从 base64 编码结果逐字节取出
- `D =/= $=` - 过滤条件：不等于 `=` 字符
- `<< (urlencode_digit(D)) >>` - 对每个字节调用转换函数

#### 列表处理分支
```erlang
encode(L) when is_list(L) ->
  encode(iolist_to_binary(L)).
```
- 将 iolist 转为二进制后递归调用

---

### 2.3 encode_mime/1 函数

```erlang
-spec encode_mime(
    binary() | iolist()
  ) -> binary().
encode_mime(Bin) when is_binary(Bin) ->
    << << (urlencode_digit(D)) >> || <<D>> <= base64:encode(Bin) >>;
encode_mime(L) when is_list(L) ->
    encode_mime(iolist_to_binary(L)).
```

**与 encode/1 的区别**：

| 函数 | 填充字符 `=` | 用途 |
|------|-------------|------|
| `encode/1` | 移除 | 纯 URL 安全 |
| `encode_mime/1` | 保留 | MIME 兼容 |

**注意**：没有 `D =/= $=` 过滤条件，所以保留了填充字符。

---

### 2.4 decode/1 函数

```erlang
-spec decode(
    binary() | iolist()
  ) -> binary().

decode(Bin) when is_binary(Bin) ->
  Bin2 = case byte_size(Bin) rem 4 of
    % 1 -> << Bin/binary, "===" >>;
    2 -> << Bin/binary, "==" >>;
    3 -> << Bin/binary, "=" >>;
    _ -> Bin
  end,
  base64:decode(<< << (urldecode_digit(D)) >> || <<D>> <= Bin2 >>);
decode(L) when is_list(L) ->
  decode(iolist_to_binary(L)).
```

**逐步解析**：

#### 步骤1: 恢复填充字符
```erlang
Bin2 = case byte_size(Bin) rem 4 of
    2 -> << Bin/binary, "==" >>;  %% 长度余2，补2个=
    3 -> << Bin/binary, "=" >>;   %% 长度余3，补1个=
    _ -> Bin                       %% 余0或已有填充
end,
```

**Base64 填充规则**：
- Base64 输出长度必须是 4 的倍数
- 余数为 1 是无效输入（被注释掉）
- 余数为 2 需要补 `==`
- 余数为 3 需要补 `=`

#### 步骤2: 字符反向替换 + 解码
```erlang
base64:decode(<< << (urldecode_digit(D)) >> || <<D>> <= Bin2 >>)
```
- 将 URL 安全字符转回标准 Base64 字符
- 调用标准库 `base64:decode/1` 解码

---

### 2.5 字符转换函数

```erlang
urlencode_digit($/) -> $_;
urlencode_digit($+) -> $-;
urlencode_digit(D)  -> D.

urldecode_digit($_) -> $/;
urldecode_digit($-) -> $+;
urldecode_digit(D)  -> D.
```

**字符映射表**：

| 方向 | `/` | `+` | 其他 |
|------|-----|-----|------|
| 编码 | `_` | `-` | 不变 |
| 解码 | `/` | `+` | 不变 |

**Erlang 字符语法**：
- `$_` 表示字符 `_` 的 ASCII 值 (95)
- `$/` 表示字符 `/` 的 ASCII 值 (47)
- `$+` 表示字符 `+` 的 ASCII 值 (43)
- `$-` 表示字符 `-` 的 ASCII 值 (45)

---

### 2.6 单元测试

```erlang
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

aim_test() ->
  %% 验证标准 base64 产生 URL 不安全字符
  ?assertNotEqual(
      binary:match(base64:encode([255,127,254,252]), [<<"=">>, <<"/">>, <<"+">>]),
      nomatch),
  %% 验证 encode/1 产生 URL 安全输出
  ?assertEqual(
      binary:match(encode([255,127,254,252]), [<<"=">>, <<"/">>, <<"+">>]),
      nomatch),
  ...
```

**测试覆盖**：
1. `aim_test/0` - 验证 URL 安全性
2. `codec_test/0` - 验证编解码无损
3. `iolist_test/0` - 验证 iolist 支持

---

## 3. 核心技术点

### 3.1 二进制推导式语法

```erlang
<< <<Expr>> || <<Pattern>> <= Generator, Filter >>
```

等价的列表推导式：
```erlang
[Expr || Pattern <- Generator, Filter]
```

**关键区别**：
- `<=` 用于二进制生成器（vs `<-` 用于列表）
- 输出需要双层 `<< <<...>> >>`

### 3.2 Guard 子句

```erlang
encode(Bin) when is_binary(Bin) -> ...;
encode(L) when is_list(L) -> ...
```

- `when is_binary(Bin)` - 模式匹配后的额外条件
- 实现函数重载/多态

### 3.3 iolist 类型

```erlang
-type iolist() :: maybe_improper_list(
    byte() | binary() | iolist(),
    binary() | []
).
```

示例：
```erlang
["Hello", <<" ">>, ["World", <<"!">>]]  %% 有效 iolist
iolist_to_binary(["Hello", <<" ">>, "World"]).
%% 结果: <<"Hello World">>
```

---

## 4. 使用场景

### 4.1 JWT Token 编码

```erlang
%% JWT 的三个部分都使用 base64url 编码
Header = base64url:encode(jsx:encode(#{alg => <<"HS256">>, typ => <<"JWT">>})),
Payload = base64url:encode(jsx:encode(#{sub => <<"1234567890">>})),
Signature = base64url:encode(crypto:mac(hmac, sha256, Secret, Data)).
```

### 4.2 URL 参数传递

```erlang
%% 安全地在 URL 中传递二进制数据
BinaryData = <<255, 0, 128, 64>>,
SafeParam = base64url:encode(BinaryData),
URL = <<"https://example.com/api?data=", SafeParam/binary>>.
```

### 4.3 文件名安全编码

```erlang
%% 将任意数据编码为安全的文件名
FileName = base64url:encode(term_to_binary({user, 123, "data"})).
```

---

## 5. 实战示例

### 5.1 基本编解码

```erlang
%% 简单字符串编码
Original = <<"Hello, World!">>,
Encoded = base64url:encode(Original),
Decoded = base64url:decode(Encoded),
%% Original =:= Decoded -> true

%% 二进制数据编码
BinData = <<255, 0, 128, 64, 32, 16, 8, 4, 2, 1>>,
base64url:encode(BinData).
%% 结果: <<"_wCAQCAQBAIB">>
```

### 5.2 URL 安全性验证

```erlang
%% 这组数据会产生 URL 危险字符
TestData = <<255, 127, 254, 252>>,

%% 标准 Base64
base64:encode(TestData).
%% 结果: <<"/3/+/A==">>  (包含 / + =)

%% URL 安全 Base64
base64url:encode(TestData).
%% 结果: <<"_3_-_A">>  (安全)
```

### 5.3 MIME 模式 vs 标准模式

```erlang
%% 测试不同长度的数据
Data = <<"a">>,  %% 1字节 -> 需要2个填充

base64url:encode(Data).
%% 结果: <<"YQ">>  (无填充)

base64url:encode_mime(Data).
%% 结果: <<"YQ==">>  (有填充)

%% 两者都能正确解码
base64url:decode(<<"YQ">>) =:= base64url:decode(<<"YQ==">>).
%% 结果: true
```

### 5.4 JWT-like 场景

```erlang
%% 模拟 JWT 的三个部分
HeaderJson = <<"{\"alg\":\"HS256\",\"typ\":\"JWT\"}">>,
HeaderEnc = base64url:encode(HeaderJson),
%% 结果: <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9">>

PayloadJson = <<"{\"sub\":\"1234567890\",\"name\":\"John\"}">>,
PayloadEnc = base64url:encode(PayloadJson),

SignatureBytes = crypto:strong_rand_bytes(32),
SignatureEnc = base64url:encode(SignatureBytes),

%% 组合成 JWT 格式
Token = <<HeaderEnc/binary, ".", PayloadEnc/binary, ".", SignatureEnc/binary>>.
```

---

## 6. 性能特点

| 特性 | 说明 |
|------|------|
| **零拷贝** | 二进制推导直接生成结果，无中间列表 |
| **流式处理** | 逐字节处理，内存占用低 |
| **依赖标准库** | 核心编解码使用 OTP `base64` 模块 |

---

## 7. 与其他实现对比

| 实现 | 语言 | 特点 |
|------|------|------|
| `base64url` | Erlang | 纯函数式，依赖 OTP |
| `base64.urlsafe_b64encode` | Python | 标准库内置 |
| `Base64.getUrlEncoder()` | Java | JDK 8+ 内置 |
| `btoa` + 替换 | JavaScript | 需手动处理 |

---

## 8. 在 RabbitMQ 中的应用

`base64url` 模块在 RabbitMQ 中主要用于：

1. **OAuth 2.0 / JWT 认证**：编解码 JWT Token
2. **管理 API**：URL 参数中传递二进制数据
3. **消息属性编码**：某些需要 URL 安全的场景

---

## 9. 参考资料

- [RFC 4648 - Base Encodings](https://tools.ietf.org/html/rfc4648)
- [Erlang base64 模块](https://www.erlang.org/doc/man/base64.html)
- [Binary Comprehensions](https://www.erlang.org/doc/reference_manual/expressions.html#binary-comprehensions)
- [base64url GitHub](https://github.com/dvv/base64url)
