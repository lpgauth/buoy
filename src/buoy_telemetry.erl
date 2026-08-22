-module(buoy_telemetry).
-include("buoy_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    request_error/3,
    request_sent/3
]).

%% internal
-spec request_error(method(), host(), atom()) ->
    ok.

request_error(Method, Host, Reason) ->
    case enabled() of
        true ->
            telemetry:execute([buoy, request, error], #{count => 1},
                #{method => Method, host => Host, reason => Reason});
        false ->
            ok
    end.

-spec request_sent(method(), host(), boolean()) ->
    ok.

request_sent(Method, Host, Async) ->
    case enabled() of
        true ->
            telemetry:execute([buoy, request, sent], #{count => 1},
                #{method => Method, host => Host, async => Async});
        false ->
            ok
    end.

%% private
enabled() ->
    persistent_term:get({buoy, telemetry}, true).
