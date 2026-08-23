-module(buoy_tests).
-include_lib("buoy/include/buoy.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BASE_URL, <<"http://127.0.0.1:8080">>).
-define(BASE_URL_SSL, <<"https://127.0.0.1:8443">>).

-define(RESP_1, #buoy_resp {status_code = 200, content_length = 12}).
-define(RESP_2, #buoy_resp {status_code = 200, content_length = 12000}).
-define(RESP_3, #buoy_resp {status_code = 200, content_length = 0}).
-define(RESP_4, #buoy_resp {status_code = 200, content_length = chunked}).

-define(VERB, <<"DEFROBNICATE">>). % because 12 characters

%% the test cert is self-signed, so verification must be off
-define(SSL_SOCKET_OPTIONS, [
    binary,
    {packet, raw},
    {verify, verify_none}
]).

%% runners
buoy_test_() ->
    {setup,
        fun () -> setup() end,
        fun (_) -> cleanup() end,
    [
        fun () -> custom_subtest(?BASE_URL) end,
        fun () -> get_subtest(?BASE_URL) end,
        fun () -> pool_subtest(?BASE_URL) end,
        fun () -> post_subtest(?BASE_URL) end,
        fun () -> put_subtest(?BASE_URL) end,
        fun () -> head_subtest(?BASE_URL) end,
        fun () -> telemetry_sent_subtest(?BASE_URL) end,
        fun () -> telemetry_error_subtest() end,
        fun () -> telemetry_disabled_subtest(?BASE_URL) end
    ]}.

buoy_https_test_() ->
    {setup,
        fun () -> setup(?BASE_URL_SSL,
            [{socket_options, ?SSL_SOCKET_OPTIONS}]) end,
        fun (_) -> cleanup(?BASE_URL_SSL) end,
    [
        fun () -> custom_subtest(?BASE_URL_SSL) end,
        fun () -> get_subtest(?BASE_URL_SSL) end,
        fun () -> post_subtest(?BASE_URL_SSL) end,
        fun () -> put_subtest(?BASE_URL_SSL) end,
        fun () -> head_subtest(?BASE_URL_SSL) end
    ]}.

buoy_https_socket_test_() ->
    case otp_release() >= 28 of
        true ->
            {setup,
                fun () -> setup(?BASE_URL_SSL, [
                    {protocol, shackle_ssl_socket},
                    {socket_options, ?SSL_SOCKET_OPTIONS}]) end,
                fun (_) -> cleanup(?BASE_URL_SSL) end,
            [
                fun () -> custom_subtest(?BASE_URL_SSL) end,
                fun () -> get_subtest(?BASE_URL_SSL) end,
                fun () -> post_subtest(?BASE_URL_SSL) end,
                fun () -> put_subtest(?BASE_URL_SSL) end,
                fun () -> head_subtest(?BASE_URL_SSL) end
            ]};
        false ->
            []
    end.

buoy_max_requests_test_() ->
    {setup,
        fun () -> setup([{max_requests, 2}, {pool_size, 1}]) end,
        fun (_) -> cleanup() end,
    [
        fun () -> max_requests_subtest(?BASE_URL) end
    ]}.

buoy_socket_test_() ->
    case otp_release() >= 28 of
        true ->
            {setup,
                fun () -> setup([{protocol, shackle_socket}]) end,
                fun (_) -> cleanup() end,
            [
                fun () -> custom_subtest(?BASE_URL) end,
                fun () -> get_subtest(?BASE_URL) end,
                fun () -> post_subtest(?BASE_URL) end,
                fun () -> put_subtest(?BASE_URL) end,
                fun () -> head_subtest(?BASE_URL) end
            ]};
        false ->
            []
    end.

%% tests
custom_subtest(BaseUrl) ->
    {ok, ReqId} = buoy:async_custom(?VERB, url(BaseUrl, <<"/5">>), #{}),
    {ok, ?RESP_1} = buoy:receive_response(ReqId),
    {ok, ?RESP_1} = buoy:custom(<<"GET">>, url(BaseUrl, <<"/1">>), #{}),
    {ok, ?RESP_3} = buoy:custom(<<"POST">>, url(BaseUrl, <<"/3">>), #{}),
    {ok, ?RESP_1} = buoy:custom(?VERB, url(BaseUrl, <<"/5">>), #{}).

get_subtest(BaseUrl) ->
    {ok, ReqId} = buoy:async_get(url(BaseUrl, <<"/1">>), #{}),
    {ok, ?RESP_1} = buoy:receive_response(ReqId),
    {ok, ?RESP_1} = buoy:get(url(BaseUrl, <<"/1">>), #{}),
    {ok, ?RESP_2} = buoy:get(url(BaseUrl, <<"/2">>), #{}),
    {ok, ?RESP_4} = buoy:get(url(BaseUrl, <<"/4">>), #{}).

max_requests_subtest(BaseUrl) ->
    1 = buoy_http_server:connection_count(),
    lists:foreach(fun (_) ->
        {ok, ?RESP_1} = buoy:get(url(BaseUrl, <<"/1">>), #{}),
        {ok, ?RESP_1} = buoy:get(url(BaseUrl, <<"/1">>), #{}),
        %% recycling closes the socket after the 2nd response and
        %% reconnects asynchronously; wait it out before the next pair
        timer:sleep(100)
    end, lists:seq(1, 3)),
    true = buoy_http_server:connection_count() >= 3.

pool_subtest(BaseUrl) ->
    Url = url(BaseUrl, <<"/1">>),
    {error, pool_already_started} = buoy_pool:start(Url),
    ok = buoy_pool:stop(Url),
    {error, pool_not_started} = buoy_pool:stop(Url),
    {error, pool_not_started} = buoy:get(Url, #{}),
    {error, pool_not_started} = buoy:async_get(Url, #{}),
    ok = buoy_pool:start(Url).

post_subtest(BaseUrl) ->
    {ok, ReqId} = buoy:async_post(url(BaseUrl, <<"/3">>), #{}),
    {ok, ?RESP_3} = buoy:receive_response(ReqId),
    {ok, ?RESP_3} = buoy:post(url(BaseUrl, <<"/3">>), #{}),
    {ok, ?RESP_1} = buoy:post(url(BaseUrl, <<"/3">>),
        #{body => <<"Hello world!">>}).

put_subtest(BaseUrl) ->
    {ok, ReqId} = buoy:async_put(url(BaseUrl, <<"/3">>), #{}),
    {ok, ?RESP_3} = buoy:receive_response(ReqId),
    {ok, ?RESP_3} = buoy:put(url(BaseUrl, <<"/3">>), #{}),
    {ok, ?RESP_1} = buoy:put(url(BaseUrl, <<"/3">>),
        #{body => <<"Hello world!">>}).

head_subtest(BaseUrl) ->
    {ok, ReqId} = buoy:async_head(url(BaseUrl, <<"/1">>), #{}),
    {ok, ?RESP_1} = buoy:receive_response(ReqId),
    {ok, ?RESP_1} = buoy:head(url(BaseUrl, <<"/1">>), #{}),
    {ok, ?RESP_2} = buoy:head(url(BaseUrl, <<"/2">>), #{}),
    {ok, ?RESP_4} = buoy:head(url(BaseUrl, <<"/4">>), #{}).

telemetry_sent_subtest(BaseUrl) ->
    Self = self(),
    HandlerId = <<"buoy-test-sent">>,
    ok = telemetry:attach(HandlerId, [buoy, request, sent],
        fun (Event, Measurements, Metadata, _) ->
            Self ! {telemetry, Event, Measurements, Metadata}
        end, undefined),
    try
        {ok, ?RESP_1} = buoy:get(url(BaseUrl, <<"/1">>), #{}),
        receive
            {telemetry, [buoy, request, sent],
             #{count := 1},
             #{method := get, async := false}} -> ok
        after 1000 ->
            erlang:error(timeout_waiting_for_sent_event)
        end
    after
        telemetry:detach(HandlerId)
    end.

telemetry_error_subtest() ->
    Self = self(),
    HandlerId = <<"buoy-test-error">>,
    ok = telemetry:attach(HandlerId, [buoy, request, error],
        fun (Event, Measurements, Metadata, _) ->
            Self ! {telemetry, Event, Measurements, Metadata}
        end, undefined),
    try
        UnstartedUrl = buoy_utils:parse_url(<<"http://127.0.0.1:9999/x">>),
        {error, pool_not_started} = buoy:get(UnstartedUrl, #{}),
        receive
            {telemetry, [buoy, request, error],
             #{count := 1},
             #{method := get, reason := pool_not_started}} -> ok
        after 1000 ->
            erlang:error(timeout_waiting_for_error_event)
        end
    after
        telemetry:detach(HandlerId)
    end.

telemetry_disabled_subtest(BaseUrl) ->
    Self = self(),
    HandlerId = <<"buoy-test-disabled">>,
    ok = telemetry:attach(HandlerId, [buoy, request, sent],
        fun (Event, Measurements, Metadata, _) ->
            Self ! {telemetry, Event, Measurements, Metadata}
        end, undefined),
    persistent_term:put({buoy, telemetry}, false),
    try
        {ok, ?RESP_1} = buoy:get(url(BaseUrl, <<"/1">>), #{}),
        receive
            {telemetry, _, _, _} ->
                erlang:error(unexpected_telemetry_event)
        after 200 ->
            ok
        end
    after
        persistent_term:put({buoy, telemetry}, true),
        telemetry:detach(HandlerId)
    end.

%% utils
cleanup() ->
    cleanup(?BASE_URL).

cleanup(BaseUrl) ->
    buoy_pool:stop(url(BaseUrl, <<"/1">>)),
    buoy_app:stop(),
    buoy_http_server:stop().

otp_release() ->
    list_to_integer(erlang:system_info(otp_release)).

setup() ->
    setup([]).

setup(PoolOptions) ->
    setup(?BASE_URL, PoolOptions).

setup(BaseUrl, PoolOptions) ->
    error_logger:tty(false),
    {ok, _} = buoy_http_server:start(),
    timer:sleep(200),
    buoy_app:start(),
    buoy_pool:start(url(BaseUrl, <<"/1">>), PoolOptions),
    timer:sleep(200).

url(BaseUrl, Path) ->
    buoy_utils:parse_url(<<BaseUrl/binary, Path/binary>>).
