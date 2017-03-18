-module(buoy_utils_tests).
-include_lib("buoy/include/buoy.hrl").
-include_lib("eunit/include/eunit.hrl").

%% runners
parse_url_test() ->
    assert_parse_url(#buoy_url {
        protocol = http,
        host = <<"adgear.com">>,
        hostname = <<"adgear.com">>,
        port = 80,
        path = <<"/">>
    }, <<"http://adgear.com">>),

    assert_parse_url(#buoy_url {
        protocol = https,
        host = <<"adgear.com">>,
        hostname = <<"adgear.com">>,
        port = 443,
        path = <<"/hello/world">>
    }, <<"https://adgear.com/hello/world">>),

    assert_parse_url(#buoy_url {
        protocol = http,
        host = <<"test.com:8080">>,
        hostname = <<"test.com">>,
        port = 8080,
        path = <<"/">>
    }, <<"http://test.com:8080">>),

    assert_parse_url({error, invalid_url},
        <<"://test.com">>).

%% private
assert_parse_url(Expected, Url) ->
    ?assertEqual(Expected, buoy_utils:parse_url(Url)).
