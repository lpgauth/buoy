-module(buoy_http_server).

-export([
    start_http/0,
    start_https/0,
    stop_http/0,
    stop_https/0
]).

-export([
    init/2
]).

%% public
start_http() ->
    application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([{'_', [
        {"/:test", ?MODULE, []}]}]),
    {ok, _} = cowboy:start_clear(buoy_http, [{port, 8080}], #{
        env => #{dispatch => Dispatch},
        max_keepalive => infinity,
        request_timeout => infinity
    }).

start_https() ->
    application:ensure_all_started(cowboy),
    LibDir = code:lib_dir(buoy),
    io:format("~p~n", [LibDir ++ "/test/data/TestCA.crt"]),
    Dispatch = cowboy_router:compile([{'_', [
        {"/:test", ?MODULE, []}]}]),
    {ok, _} = cowboy:start_tls(buoy_https, [
        {port, 8081},
        {cacertfile, LibDir ++ "/test/data/TestCA.crt"},
        {certfile, LibDir ++ "/test/data/localhost.crt"},
        {keyfile, LibDir ++ "/test/data/localhost.key"}
    ], #{
        env => #{dispatch => Dispatch},
        max_keepalive => infinity,
        request_timeout => infinity
    }).
    
stop_http() ->
    cowboy:stop_listener(buoy_http).

stop_https() ->
    cowboy:stop_listener(buoy_https).
    
%% cowboy callbacks
init(Req, State) ->
    case cowboy_req:binding(test, Req) of
        <<"1">> ->
            reply(200, <<"Hello world!">>, Req, State);
        <<"2">> ->
            Body = [<<"Hello world!">> || _ <- lists:seq(1, 1000)],
            reply(200, Body, Req, State);
        <<"3">> ->
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            reply(200, Body, Req2, State);
        <<"4">> ->
            Req2 = cowboy_req:stream_reply(200, Req),
            ok = cowboy_req:stream_body("Hello", nofin, Req2),
            ok = cowboy_req:stream_body(" world!", fin, Req2),
            {ok, Req2, State};
        <<"5">> ->
            Verb = cowboy_req:method(Req),
            reply(200, Verb, Req, State)
    end.

%% private
reply(StatusCode, Body, Req, State) ->
    Req2 = cowboy_req:reply(StatusCode, #{
        <<"Content-Type">> => <<"text/plain">>,
        <<"Connection">> => <<"Keep-Alive">>
    }, Body, Req),
    {ok, Req2, State}.
