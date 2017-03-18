-module(buoy_top_domains_test).

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
    Url = buoy_utils:parse_url(<<"http://", Domain/binary, "/">>),
    buoy_pool:start(Url, [
        {pool_size, 1},
        {reconnect, false}
    ]),
    buoy:get(Url),
    ok = buoy_pool:stop(Url).

loop_file(File) ->
    case file:read_line(File) of
        {ok, Line} ->
            Line2 = binary:part(Line, {0, size(Line) - 1}),
            domain_test(Line2),
            loop_file(File);
        eof ->
            ok
    end.
