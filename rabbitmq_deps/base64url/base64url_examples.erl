%%%-------------------------------------------------------------------
%%% @doc Base64url 模块使用示例
%%% 
%%% 编译运行:
%%%   $ erlc base64url_examples.erl
%%%   $ erl -pa . -pa /path/to/base64url/ebin
%%%   > base64url_examples:run_all().
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(base64url_examples).

-export([
    run_all/0,
    example_basic_encode_decode/0,
    example_url_safety/0,
    example_mime_vs_standard/0,
    example_iolist_support/0,
    example_jwt_like/0,
    example_binary_comprehension/0
]).

%%--------------------------------------------------------------------
%% @doc 运行所有示例
%%--------------------------------------------------------------------
run_all() ->
    io:format("~n=== Base64url 模块示例演示 ===~n~n"),
    example_basic_encode_decode(),
    example_url_safety(),
    example_mime_vs_standard(),
    example_iolist_support(),
    example_jwt_like(),
    example_binary_comprehension(),
    io:format("=== 所有示例完成 ===~n~n").

%%--------------------------------------------------------------------
%% @doc 示例1: 基本编解码
%%--------------------------------------------------------------------
example_basic_encode_decode() ->
    io:format("--- 示例1: 基本编解码 ---~n"),
    
    %% 简单字符串编码
    Original1 = <<"Hello, World!">>,
    Encoded1 = base64url:encode(Original1),
    Decoded1 = base64url:decode(Encoded1),
    io:format("原始数据: ~p~n", [Original1]),
    io:format("编码结果: ~p~n", [Encoded1]),
    io:format("解码结果: ~p~n", [Decoded1]),
    io:format("编解码一致: ~p~n~n", [Original1 =:= Decoded1]),
    
    %% 二进制数据编码
    Original2 = <<255, 0, 128, 64, 32, 16, 8, 4, 2, 1>>,
    Encoded2 = base64url:encode(Original2),
    Decoded2 = base64url:decode(Encoded2),
    io:format("二进制原始: ~p~n", [Original2]),
    io:format("二进制编码: ~p~n", [Encoded2]),
    io:format("解码验证: ~p~n~n", [Original2 =:= Decoded2]).

%%--------------------------------------------------------------------
%% @doc 示例2: URL 安全性对比
%%--------------------------------------------------------------------
example_url_safety() ->
    io:format("--- 示例2: URL 安全性对比 ---~n"),
    
    %% 这组数据会产生 URL 危险字符
    TestData = <<255, 127, 254, 252>>,
    
    %% 标准 Base64
    StdEncoded = base64:encode(TestData),
    io:format("标准 Base64: ~p~n", [StdEncoded]),
    
    %% URL 安全 Base64
    UrlEncoded = base64url:encode(TestData),
    io:format("URL安全版本: ~p~n", [UrlEncoded]),
    
    %% 检查危险字符
    DangerChars = [<<"/">>, <<"+">>, <<"=">>],
    StdHasDanger = lists:any(
        fun(C) -> binary:match(StdEncoded, C) =/= nomatch end,
        DangerChars
    ),
    UrlHasDanger = lists:any(
        fun(C) -> binary:match(UrlEncoded, C) =/= nomatch end,
        DangerChars
    ),
    
    io:format("标准版含危险字符: ~p~n", [StdHasDanger]),
    io:format("URL版含危险字符: ~p~n~n", [UrlHasDanger]),
    
    %% 字符替换对照
    io:format("字符替换对照:~n"),
    io:format("  标准 '/' (ASCII ~p) -> URL '_' (ASCII ~p)~n", [$/, $_]),
    io:format("  标准 '+' (ASCII ~p) -> URL '-' (ASCII ~p)~n", [$+, $-]),
    io:format("  标准 '=' (填充)    -> URL 省略~n~n").

%%--------------------------------------------------------------------
%% @doc 示例3: MIME 模式 vs 标准模式
%%--------------------------------------------------------------------
example_mime_vs_standard() ->
    io:format("--- 示例3: MIME模式 vs 标准模式 ---~n"),
    
    %% 测试不同长度的数据（会产生不同数量的填充）
    TestCases = [
        {<<"a">>,     "1字节 -> 需要2个填充"},
        {<<"ab">>,    "2字节 -> 需要1个填充"},
        {<<"abc">>,   "3字节 -> 无需填充"},
        {<<"abcd">>,  "4字节 -> 需要2个填充"}
    ],
    
    lists:foreach(
        fun({Data, Desc}) ->
            StdEnc = base64url:encode(Data),
            MimeEnc = base64url:encode_mime(Data),
            io:format("~s:~n", [Desc]),
            io:format("  原始: ~p~n", [Data]),
            io:format("  标准模式 (无填充): ~p~n", [StdEnc]),
            io:format("  MIME模式 (有填充): ~p~n", [MimeEnc]),
            io:format("  两者都能正确解码: ~p~n~n", [
                base64url:decode(StdEnc) =:= base64url:decode(MimeEnc)
            ])
        end,
        TestCases
    ).

%%--------------------------------------------------------------------
%% @doc 示例4: iolist 支持
%%--------------------------------------------------------------------
example_iolist_support() ->
    io:format("--- 示例4: iolist 支持 ---~n"),
    
    %% 字符串列表 (Erlang 字符串就是整数列表)
    String = "Hello",
    Encoded1 = base64url:encode(String),
    io:format("字符串 ~p -> ~p~n", [String, Encoded1]),
    
    %% 嵌套 iolist
    IoList = ["Hel", <<"lo">>, [" ", <<"World">>]],
    Encoded2 = base64url:encode(IoList),
    io:format("iolist ~p~n  -> ~p~n", [IoList, Encoded2]),
    
    %% 验证与直接编码二进制结果一致
    DirectEnc = base64url:encode(<<"Hello World">>),
    io:format("直接编码 <<\"Hello World\">>: ~p~n", [DirectEnc]),
    io:format("iolist编码结果一致: ~p~n~n", [Encoded2 =:= DirectEnc]),
    
    %% 字节列表 (0-255 范围的整数)
    ByteList = [255, 127, 0, 128],
    Encoded3 = base64url:encode(ByteList),
    Decoded3 = base64url:decode(Encoded3),
    io:format("字节列表 ~p -> ~p -> ~p~n~n", [ByteList, Encoded3, Decoded3]).

%%--------------------------------------------------------------------
%% @doc 示例5: JWT-like 场景
%%--------------------------------------------------------------------
example_jwt_like() ->
    io:format("--- 示例5: JWT-like 场景 ---~n"),
    
    %% 模拟 JWT 的三个部分
    %% Header
    HeaderJson = <<"{\"alg\":\"HS256\",\"typ\":\"JWT\"}">>,
    HeaderEnc = base64url:encode(HeaderJson),
    
    %% Payload
    PayloadJson = <<"{\"sub\":\"1234567890\",\"name\":\"John\"}">>,
    PayloadEnc = base64url:encode(PayloadJson),
    
    %% Signature (模拟)
    SignatureBytes = crypto:strong_rand_bytes(32),
    SignatureEnc = base64url:encode(SignatureBytes),
    
    %% 组合成 JWT 格式
    Token = <<HeaderEnc/binary, ".", PayloadEnc/binary, ".", SignatureEnc/binary>>,
    
    io:format("Header JSON: ~s~n", [HeaderJson]),
    io:format("Header Encoded: ~s~n~n", [HeaderEnc]),
    
    io:format("Payload JSON: ~s~n", [PayloadJson]),
    io:format("Payload Encoded: ~s~n~n", [PayloadEnc]),
    
    io:format("Signature (32 random bytes):~n  ~p~n", [SignatureEnc]),
    
    io:format("~n完整 Token:~n  ~s~n~n", [Token]),
    
    %% 解析 Token
    [H, P, _S] = binary:split(Token, <<".">>, [global]),
    io:format("解析验证:~n"),
    io:format("  Header解码: ~s~n", [base64url:decode(H)]),
    io:format("  Payload解码: ~s~n~n", [base64url:decode(P)]).

%%--------------------------------------------------------------------
%% @doc 示例6: 二进制推导式原理演示
%%--------------------------------------------------------------------
example_binary_comprehension() ->
    io:format("--- 示例6: 二进制推导式原理 ---~n"),
    
    %% 演示 urlencode_digit 的工作原理
    Input = <<"/3/+/A==">>,  %% 标准 Base64 输出
    
    io:format("输入: ~p~n", [Input]),
    io:format("逐字节转换过程:~n"),
    
    %% 手动展开推导式过程
    lists:foreach(
        fun(Byte) ->
            Converted = urlencode_digit(Byte),
            if
                Byte =/= Converted ->
                    io:format("  ~c (ASCII ~p) -> ~c (ASCII ~p)~n", 
                              [Byte, Byte, Converted, Converted]);
                Byte =:= $= ->
                    io:format("  ~c (ASCII ~p) -> [移除]~n", [Byte, Byte]);
                true ->
                    io:format("  ~c (ASCII ~p) -> 保持不变~n", [Byte, Byte])
            end
        end,
        binary_to_list(Input)
    ),
    
    %% 等价的二进制推导式
    Output = << << (urlencode_digit(D)) >> || <<D>> <= Input, D =/= $= >>,
    io:format("输出: ~p~n~n", [Output]),
    
    %% 解释推导式语法
    io:format("二进制推导式语法解析:~n"),
    io:format("  << <<(urlencode_digit(D))>> || <<D>> <= Input, D =/= $= >>~n"),
    io:format("  ├── <<D>> <= Input     : 从 Input 逐字节取出赋给 D~n"),
    io:format("  ├── D =/= $=           : 过滤条件，排除 '=' 字符~n"),
    io:format("  └── <<(urlencode_digit(D))>> : 转换后重新组装成二进制~n~n").

%%--------------------------------------------------------------------
%% 内部函数：字符转换（与 base64url 模块中相同）
%%--------------------------------------------------------------------
urlencode_digit($/) -> $_;
urlencode_digit($+) -> $-;
urlencode_digit(D)  -> D.
