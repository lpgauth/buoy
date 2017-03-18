-module(buoy_profile).

-export([
    fprofx/0
]).

-define(N, 1000).
-define(P, 20).

%% public
-spec fprofx() -> ok.

fprofx() ->
    Filenames = filelib:wildcard("_build/default/lib/*/ebin/*.beam"),
    Rootnames = [filename:rootname(Filename, ".beam") ||
        Filename <- Filenames],
    lists:foreach(fun code:load_abs/1, Rootnames),

    BuoyUrl = buoy_utils:parse_url(<<"http://127.0.0.1:8080/1">>),

    fprofx:start(),
    {ok, Tracer} = fprofx:profile(start),
    fprofx:trace([start, {procs, new}, {tracer, Tracer}]),

    buoy_app:start(),
    buoy_pool:start(BuoyUrl),

    Self = self(),
    [spawn(fun () ->
        [{ok, _} = buoy_get(BuoyUrl) || _ <- lists:seq(1, ?N)],
        Self ! exit
    end) || _ <- lists:seq(1, ?P)],
    wait(),

    fprofx:trace(stop),
    fprofx:analyse([totals, {dest, ""}]),
    fprofx:stop(),

    buoy_app:stop(),
    ok.

%% private
buoy_get(BuoyUrl) ->
    {ok, _} = buoy:get(BuoyUrl).

wait() ->
    wait(?P).

wait(0) ->
    ok;
wait(X) ->
    receive
        exit ->
            wait(X - 1)
    end.
