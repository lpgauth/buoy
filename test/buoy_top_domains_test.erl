-module(buoy_top_domains_test).
-include("test.hrl").

-export([
    run/0
]).

-define(DOMAIN_FILE, "test/data/domains_10000").

%% public
run() ->
    {ok, _} = buoy_app:start(),
    {ok, File} = file:open(?DOMAIN_FILE, [binary, read]),
    loop_file(File).

%% private
domain_test(Domain) ->
    buoy_pool:start(Domain, 80, [
        {pool_size, 1},
        {reconnect, false}
    ]),
    case buoy:get(<<"http://", Domain/binary, "/">>) of
        {ok, #buoy_resp {}} ->
            ok;
        {error, unsupported_feature} ->
            ok;
        {error, Reason} ->
            io:format("~p: ~p~n", [Domain, Reason])
    end,
    ok = buoy_pool:stop(Domain, 80).

loop_file(File) ->
    case file:read_line(File) of
        {ok, Line} ->
            Line2 = binary:part(Line, {0, size(Line) - 1}),
            domain_test(Line2),
            loop_file(File);
        eof ->
            ok
    end.
