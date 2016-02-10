-module(buoy_http_server).
-include("test.hrl").

-export([
    start/0,
    stop/0
]).

-behavior(cowboy_http_handler).
-export([
    init/3,
    handle/2,
    terminate/3
]).

%% pbulic
start() ->
    application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([{'_', [
        {"/:test", ?MODULE, []}]}]),
    {ok, _} = cowboy:start_http(?MODULE, 16, [{port, 8080}], [
        {env, [{dispatch, Dispatch}]},
        {max_keepalive, infinity},
        {timeout, infinity}
    ]).

stop() ->
    cowboy:stop_listener(?MODULE).

%% cowboy_handler callbacks
init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    case cowboy_req:binding(test, Req) of
        {<<"1">>, Req2} ->
            reply(200, <<"Hello world!">>, Req2, State);
        {<<"2">>, Req2} ->
            Body = [<<"Hello world!">> || _ <- lists:seq(1, 1000)],
            reply(200, Body, Req2, State);
        {<<"3">>, Req2} ->
            {ok, Body, Req3} = cowboy_req:body(Req2),
            reply(200, Body, Req3, State)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%% private
reply(StatusCode, Body, Req, State) ->
    {ok, Req2} = cowboy_req:reply(StatusCode, [
        {<<"Content-Type">>, <<"text/plain">>},
        {<<"Connection">>, <<"Keep-Alive">>}
    ], Body, Req),
    {ok, Req2, State}.
