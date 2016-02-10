-module(buoy_tests).
-include("test.hrl").

%% runners
buoy_test_() ->
    {setup,
        fun () -> setup() end,
        fun (_) -> cleanup() end,
    [
        fun get_subtest/0,
        fun pool_subtest/0,
        fun post_subtest/0
    ]}.

-define(RESP_1, #buoy_resp {status_code = 200, content_length = 12}).
-define(RESP_2, #buoy_resp {status_code = 200, content_length = 12000}).
-define(RESP_3, #buoy_resp {status_code = 200, content_length = 0}).

-define(URL_1, <<"http://127.0.0.1:8080/1">>).
-define(URL_2, <<"http://127.0.0.1:8080/2">>).
-define(URL_3, <<"http://127.0.0.1:8080/3">>).

-define(URL(Url), buoy_utils:parse_url(Url)).

%% tests
get_subtest() ->
    {ok, ReqId} = buoy:async_get(?URL(?URL_1)),
    {ok, ?RESP_1} = buoy:receive_response(ReqId),
    {ok, ?RESP_1} = buoy:get(?URL(?URL_1)),
    {ok, ?RESP_2} = buoy:get(?URL(?URL_2)).

pool_subtest() ->
    {error, pool_already_started} = buoy_pool:start(?HOST, ?PORT),
    ok = buoy_pool:stop(?HOST, ?PORT),
    {error, pool_not_started} = buoy_pool:stop(?HOST, ?PORT),
    {error, pool_not_started} = buoy:get(?URL(?URL_1)),
    {error, pool_not_started} = buoy:async_get(?URL(?URL_1)),
    ok = buoy_pool:start(?HOST, ?PORT).

post_subtest() ->
    {ok, ReqId} = buoy:async_post(?URL(?URL_3)),
    {ok, ?RESP_3} = buoy:receive_response(ReqId),
    {ok, ?RESP_3} = buoy:post(?URL(?URL_3)),
    {ok, ?RESP_1} = buoy:post(?URL(?URL_3), [], <<"Hello world!">>).

%% utils
cleanup() ->
    buoy_pool:stop(?HOST, ?PORT),
    buoy_app:stop(),
    buoy_http_server:stop().

setup() ->
    error_logger:tty(false),
    {ok, _} = buoy_http_server:start(),
    timer:sleep(100),
    buoy_app:start(),
    buoy_pool:start(?HOST, ?PORT).
