-module(buoy_tests).
-include_lib("buoy/include/buoy.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BASE_HTTP, <<"http://127.0.0.1:8080">>).
-define(BASE_HTTPS, <<"http://127.0.0.1:8081">>).

-define(RESP_1, #buoy_resp {status_code = 200, content_length = 12}).
-define(RESP_2, #buoy_resp {status_code = 200, content_length = 12000}).
-define(RESP_3, #buoy_resp {status_code = 200, content_length = 0}).
-define(RESP_4, #buoy_resp {status_code = 200, content_length = chunked}).

-define(VERB, <<"DEFROBNICATE">>). % because 12 characters

-define(URL_1, <<?BASE_HTTP/binary, "/1">>).
-define(URL_2, <<?BASE_HTTP/binary, "/2">>).
-define(URL_3, <<?BASE_HTTP/binary, "/3">>).
-define(URL_4, <<?BASE_HTTP/binary, "/4">>).
-define(URL_5, <<?BASE_HTTP/binary, "/5">>).
-define(URL_6, <<?BASE_HTTPS/binary, "/1">>).

-define(URL(Url), buoy_utils:parse_url(Url)).

%% runners
buoy_test_() ->
    {setup,
        fun () -> setup() end,
        fun (_) -> cleanup() end,
    [
        fun custom_subtest/0,
        fun get_subtest/0,
        fun pool_subtest/0,
        fun post_subtest/0,
        fun put_subtest/0,
        fun head_subtest/0
    ]}.

%% tests
custom_subtest() ->
    {ok, ReqId} = buoy:async_custom(?VERB, ?URL(?URL_5), #{}),
    {ok, ?RESP_1} = buoy:receive_response(ReqId),
    {ok, ?RESP_1} = buoy:custom(<<"GET">>, ?URL(?URL_1), #{}),
    {ok, ?RESP_3} = buoy:custom(<<"POST">>, ?URL(?URL_3), #{}),
    {ok, ?RESP_1} = buoy:custom(?VERB, ?URL(?URL_5), #{}),
    {ok, ?RESP_1} = buoy:custom(?VERB, ?URL(?URL_6), #{}).
    

get_subtest() ->
    {ok, ReqId} = buoy:async_get(?URL(?URL_1), #{}),
    {ok, ?RESP_1} = buoy:receive_response(ReqId),
    {ok, ?RESP_1} = buoy:get(?URL(?URL_1), #{}),
    {ok, ?RESP_2} = buoy:get(?URL(?URL_2), #{}),
    {ok, ?RESP_4} = buoy:get(?URL(?URL_4), #{}),
    {ok, ?RESP_1} = buoy:get(?URL(?URL_6), #{}).

pool_subtest() ->
    {error, pool_already_started} = buoy_pool:start(?URL(?URL_1)),
    ok = buoy_pool:stop(?URL(?URL_1)),
    {error, pool_not_started} = buoy_pool:stop(?URL(?URL_1)),
    {error, pool_not_started} = buoy:get(?URL(?URL_1), #{}),
    {error, pool_not_started} = buoy:async_get(?URL(?URL_1), #{}),
    ok = buoy_pool:start(?URL(?URL_1)).

post_subtest() ->
    {ok, ReqId} = buoy:async_post(?URL(?URL_3), #{}),
    {ok, ?RESP_3} = buoy:receive_response(ReqId),
    {ok, ?RESP_3} = buoy:post(?URL(?URL_3), #{}),
    {ok, ?RESP_1} = buoy:post(?URL(?URL_3), #{body => <<"Hello world!">>}).

put_subtest() ->
    {ok, ReqId} = buoy:async_put(?URL(?URL_3), #{}),
    {ok, ?RESP_3} = buoy:receive_response(ReqId),
    {ok, ?RESP_3} = buoy:put(?URL(?URL_3), #{}),
    {ok, ?RESP_1} = buoy:put(?URL(?URL_3), #{body => <<"Hello world!">>}).

head_subtest() ->
    {ok, ReqId} = buoy:async_head(?URL(?URL_1), #{}),
    {ok, ?RESP_1} = buoy:receive_response(ReqId),
    {ok, ?RESP_1} = buoy:head(?URL(?URL_1), #{}),
    {ok, ?RESP_2} = buoy:head(?URL(?URL_2), #{}),
    {ok, ?RESP_4} = buoy:head(?URL(?URL_4), #{}),
    {ok, ?RESP_1} = buoy:head(?URL(?URL_6), #{}).

%% utils
cleanup() ->
    buoy_pool:stop(?URL(?URL_1)),
    buoy_pool:stop(?URL(?URL_6)),
    buoy_app:stop(),
    buoy_http_server:stop_http(),
    buoy_http_server:stop_https().

setup() ->
    % error_logger:tty(false),
    {ok, _} = buoy_http_server:start_http(),
    {ok, _} = buoy_http_server:start_https(),
    timer:sleep(200),
    buoy_app:start(),
    buoy_pool:start(?URL(?URL_1)),
    buoy_pool:start(?URL(?URL_6)),
    timer:sleep(200).
