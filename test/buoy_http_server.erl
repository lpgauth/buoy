-module(buoy_http_server).

-export([
    start/0,
    stop/0
]).

-export([
    init/2
]).

%% public
start() ->
    application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile([{'_', [
        {"/:test", ?MODULE, []}]}]),
    {ok, _} = cowboy:start_clear(?MODULE, [{port, 8080}], #{
        env => #{dispatch => Dispatch},
        max_keepalive => infinity,
        request_timeout => infinity
    }).

stop() ->
    cowboy:stop_listener(?MODULE).

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
