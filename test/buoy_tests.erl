-module(buoy_tests).
-include_lib("buoy/include/buoy.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(BASE_URL, <<"http://127.0.0.1:8080">>).

-define(RESP_1, #buoy_resp {status_code = 200, content_length = 12}).
-define(RESP_2, #buoy_resp {status_code = 200, content_length = 12000}).
-define(RESP_3, #buoy_resp {status_code = 200, content_length = 0}).
-define(RESP_4, #buoy_resp {status_code = 200, content_length = chunked}).

-define(VERB, <<"DEFROBNICATE">>). % because 12 characters

-define(URL_1, <<?BASE_URL/binary, "/1">>).
-define(URL_2, <<?BASE_URL/binary, "/2">>).
-define(URL_3, <<?BASE_URL/binary, "/3">>).
-define(URL_4, <<?BASE_URL/binary, "/4">>).
-define(URL_5, <<?BASE_URL/binary, "/5">>).

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
        fun head_subtest/0,
        fun telemetry_sent_subtest/0,
        fun telemetry_error_subtest/0,
        fun telemetry_disabled_subtest/0
    ]}.

buoy_max_requests_test_() ->
    {setup,
        fun () -> setup([{max_requests, 2}, {pool_size, 1}]) end,
        fun (_) -> cleanup() end,
    [
        fun max_requests_subtest/0
    ]}.

buoy_socket_test_() ->
    case list_to_integer(erlang:system_info(otp_release)) >= 28 of
        true ->
            {setup,
                fun () -> setup([{protocol, shackle_socket}]) end,
                fun (_) -> cleanup() end,
            [
                fun custom_subtest/0,
                fun get_subtest/0,
                fun post_subtest/0,
                fun put_subtest/0,
                fun head_subtest/0
            ]};
        false ->
            []
    end.

%% tests
custom_subtest() ->
    {ok, ReqId} = buoy:async_custom(?VERB, ?URL(?URL_5), #{}),
    {ok, ?RESP_1} = buoy:receive_response(ReqId),
    {ok, ?RESP_1} = buoy:custom(<<"GET">>, ?URL(?URL_1), #{}),
    {ok, ?RESP_3} = buoy:custom(<<"POST">>, ?URL(?URL_3), #{}),
    {ok, ?RESP_1} = buoy:custom(?VERB, ?URL(?URL_5), #{}).

get_subtest() ->
    {ok, ReqId} = buoy:async_get(?URL(?URL_1), #{}),
    {ok, ?RESP_1} = buoy:receive_response(ReqId),
    {ok, ?RESP_1} = buoy:get(?URL(?URL_1), #{}),
    {ok, ?RESP_2} = buoy:get(?URL(?URL_2), #{}),
    {ok, ?RESP_4} = buoy:get(?URL(?URL_4), #{}).

max_requests_subtest() ->
    1 = buoy_http_server:connection_count(),
    lists:foreach(fun (_) ->
        {ok, ?RESP_1} = buoy:get(?URL(?URL_1), #{}),
        {ok, ?RESP_1} = buoy:get(?URL(?URL_1), #{}),
        %% recycling closes the socket after the 2nd response and
        %% reconnects asynchronously; wait it out before the next pair
        timer:sleep(100)
    end, lists:seq(1, 3)),
    true = buoy_http_server:connection_count() >= 3.

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
    {ok, ?RESP_4} = buoy:head(?URL(?URL_4), #{}).

telemetry_sent_subtest() ->
    Self = self(),
    HandlerId = <<"buoy-test-sent">>,
    ok = telemetry:attach(HandlerId, [buoy, request, sent],
        fun (Event, Measurements, Metadata, _) ->
            Self ! {telemetry, Event, Measurements, Metadata}
        end, undefined),
    try
        {ok, ?RESP_1} = buoy:get(?URL(?URL_1), #{}),
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
        UnstartedUrl = ?URL(<<"http://127.0.0.1:9999/x">>),
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

telemetry_disabled_subtest() ->
    Self = self(),
    HandlerId = <<"buoy-test-disabled">>,
    ok = telemetry:attach(HandlerId, [buoy, request, sent],
        fun (Event, Measurements, Metadata, _) ->
            Self ! {telemetry, Event, Measurements, Metadata}
        end, undefined),
    persistent_term:put({buoy, telemetry}, false),
    try
        {ok, ?RESP_1} = buoy:get(?URL(?URL_1), #{}),
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
    buoy_pool:stop(?URL(?URL_1)),
    buoy_app:stop(),
    buoy_http_server:stop().

setup() ->
    setup([]).

setup(PoolOptions) ->
    error_logger:tty(false),
    {ok, _} = buoy_http_server:start(),
    timer:sleep(200),
    buoy_app:start(),
    buoy_pool:start(?URL(?URL_1), PoolOptions),
    timer:sleep(200).
