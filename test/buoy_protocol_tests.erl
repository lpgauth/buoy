-module(buoy_protocol_tests).
-include("test.hrl").

%% runners
headers_test() ->
    {ok, Headers} = buoy_protocol:headers(#buoy_resp {
        headers = [<<"Hello: world">>, <<"Foo:">>]
    }),

    ?assertEqual([
        {<<"Hello">>, <<"world">>},
        {<<"Foo">>, undefined}
    ], Headers),

    {error, invalid_headers} = buoy_protocol:headers(#buoy_resp {
        headers = [<<"Hello: world">>, <<"Foo">>]
    }).

request_test() ->
    Request = buoy_protocol:request(get, <<"/">>, [],
        <<"127.0.0.1:8080">>, undefined),
    ?assertEqual(<<"GET / HTTP/1.1\r\nHost: 127.0.0.1:8080\r\n",
        "Connection: Keep-alive\r\nUser-Agent: buoy\r\n\r\n">>,
        iolist_to_binary(Request)),

    Request2 = buoy_protocol:request(post, <<"/">>, [],
        <<"127.0.0.1:8080">>, <<"Hello World!">>),
    ?assertEqual(<<"POST / HTTP/1.1\r\nHost: 127.0.0.1:8080\r\n",
        "Connection: Keep-alive\r\nUser-Agent: buoy\r\n",
        "Content-Length: 12\r\n\r\nHello World!\r\n">>,
        iolist_to_binary(Request2)).

response_test_() -> [
        fun response_200_subtest/0,
        fun response_204_subtest/0,
        fun response_301_subtest/0,
        fun response_302_subtest/0,
        fun response_403_subtest/0,
        fun response_404_subtest/0,
        fun response_500_subtest/0,
        fun response_502_subtest/0,
        fun response_custom_subtest/0,
        fun response_http_1_0_subtest/0
    ].

%% tests
response_200_subtest() ->
    test_response("test/data/response_200",
    #buoy_resp {
        state = done,
        status_code = 200,
        reason = <<"OK">>,
        headers = [
            <<"Server: Apache-Coyote/1.1">>,
            <<"P3P: CP=\"COM NAV INT STA NID OUR IND NOI\"">>,
            <<"Pragma: no-cache">>,
            <<"Cache-Control: no-cache">>,
            <<"Content-Type: text/html;charset=utf-8">>,
            <<"Content-Length: 164">>,
            <<"Date: Wed, 21 Dec 2016 18:29:13 GMT">>,
            <<"Connection: close">>
        ],
        content_length = 164,
        body = <<"{\"segment_ids\":[\"516\",\"513\",\"502\",\"504\",\"508\",",
            "\"510\",\"506\",\"512\",\"501\",\"503\",\"507\",\"509\",\"505\",",
            "\"511\",\"401\",\"402\",\"1500\",\"100\",\"138\",\"1582\",",
            "\"181\",\"1722\",\"531\",\"532\"]}">>
    },
    [
        {<<"Server">>, <<"Apache-Coyote/1.1">>},
        {<<"P3P">>, <<"CP=\"COM NAV INT STA NID OUR IND NOI\"">>},
        {<<"Pragma">>, <<"no-cache">>},
        {<<"Cache-Control">>, <<"no-cache">>},
        {<<"Content-Type">>, <<"text/html;charset=utf-8">>},
        {<<"Content-Length">>, <<"164">>},
        {<<"Date">>, <<"Wed, 21 Dec 2016 18:29:13 GMT">>},
        {<<"Connection">>, <<"close">>}
    ]).

response_204_subtest() ->
    test_response("test/data/response_204",
    #buoy_resp {
        state = done,
        status_code = 204,
        reason = <<"No Content">>,
        headers = [
            <<"server: Cowboy">>,
            <<"date: Wed, 15 Feb 2017 01:47:43 GMT">>,
            <<"content-length: 0">>,
            <<"Content-Type: text/plain">>,
            <<"Connection: Keep-Alive">>
        ],
        content_length = 0
    },
    [
        {<<"server">>, <<"Cowboy">>},
        {<<"date">>, <<"Wed, 15 Feb 2017 01:47:43 GMT">>},
        {<<"content-length">>, <<"0">>},
        {<<"Content-Type">>, <<"text/plain">>},
        {<<"Connection">>, <<"Keep-Alive">>}
    ]).

response_301_subtest() ->
    test_response("test/data/response_301",
    #buoy_resp {
        state = done,
        status_code = 301,
        reason = <<"Moved Permanently">>,
        headers = [
            <<"Location: https://myspace.com/">>,
            <<"Connection: close">>,
            <<"Cache-Control: no-cache">>,
            <<"Pragma: no-cache">>
        ],
        content_length = undefined,
        body = undefined
    },
    [
        {<<"Location">>, <<"https://myspace.com/">>},
        {<<"Connection">>, <<"close">>},
        {<<"Cache-Control">>, <<"no-cache">>},
        {<<"Pragma">>, <<"no-cache">>}
    ]).

response_302_subtest() ->
    test_response("test/data/response_302",
    #buoy_resp {
        state = done,
        status_code = 302,
        reason = <<"Found">>,
        headers = [
            <<"Date: Mon, 13 Feb 2017 19:54:34 GMT">>,
            <<"Server: Apache">>,
            <<"Location: http://www.noaa.gov/">>,
            <<"Vary: Accept-Encoding">>,
            <<"Content-Length: 204">>,
            <<"Content-Type: text/html; charset=iso-8859-1">>,
            <<"Via: 1.0 c5.w2.woc (squid)">>,
            <<"Connection: keep-alive">>
        ],
        content_length = 204,
        body = <<"<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">\n",
            "<html><head>\n<title>302 Found</title>\n</head><body>\n",
            "<h1>Found</h1>\n<p>The document has moved ",
            "<a href=\"http://www.noaa.gov/\">here</a>\n",
            "</p>\n</body></html>\n">>
    },
    [
        {<<"Date">>, <<"Mon, 13 Feb 2017 19:54:34 GMT">>},
        {<<"Server">>, <<"Apache">>},
        {<<"Location">>, <<"http://www.noaa.gov/">>},
        {<<"Vary">>, <<"Accept-Encoding">>},
        {<<"Content-Length">>, <<"204">>},
        {<<"Content-Type">>, <<"text/html; charset=iso-8859-1">>},
        {<<"Via">>, <<"1.0 c5.w2.woc (squid)">>},
        {<<"Connection">>, <<"keep-alive">>}
    ]).

response_403_subtest() ->
    test_response("test/data/response_403",
    #buoy_resp {
        state = done,
        status_code = 403,
        reason = <<"Forbidden">>,
        headers = [
            <<"Content-Type: application/xml">>,
            <<"Date: Tue, 14 Feb 2017 12:39:52 GMT">>,
            <<"P3P: CP=\"CAO DSP LAW CURa ADMa DEVa TAIa PSAa PSDa IVAa IVDa",
                " OUR BUS IND UNI COM NAV INT\"">>,
            <<"Server: AmazonS3">>,
            <<"x-amz-bucket-region: us-east-1">>,
            <<"Content-Length: 243">>
        ],
        content_length = 243,
        body = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>.<Error>",
            "<Code>AccessDenied</Code><Message>Access Denied</Message>",
            "<RequestId>78EBDFC1704228AB</RequestId><HostId>97RSC/NU5I/",
            "IckVPs09UZXxQypBH5MjDwPsvl4Gi7wTxxuRPXzk7TY7VWDaZrRf57gYZvM+u2Ds=",
            "</HostId></Error>">>
    },
    [
        {<<"Content-Type">>, <<"application/xml">>},
        {<<"Date">>, <<"Tue, 14 Feb 2017 12:39:52 GMT">>},
        {<<"P3P">>, <<"CP=\"CAO DSP LAW CURa ADMa DEVa TAIa PSAa PSDa IVAa",
            " IVDa OUR BUS IND UNI COM NAV INT\"">>},
        {<<"Server">>, <<"AmazonS3">>},
        {<<"x-amz-bucket-region">>, <<"us-east-1">>},
        {<<"Content-Length">>, <<"243">>}
    ]).

response_404_subtest() ->
    test_response("test/data/response_404",
    #buoy_resp {
        state = done,
        status_code = 404,
        reason = <<"Not Found">>,
        headers = [
            <<"content-type: text/html; charset=utf-8">>,
            <<"content-length: 303">>,
            <<"date: Tue, 14 Feb 2017 12:38:29 GMT">>,
            <<"server: YouTubeFrontEnd">>,
            <<"x-xss-protection: 1; mode=block">>,
            <<"x-frame-options: SAMEORIGIN">>
        ],
        content_length = 303,
        body = <<"<!DOCTYPE html><html><head><style>* ",
            "{ margin:0;padding:0;border:0}html,body{height:100%;}</style>",
            "<title>404 Not Found</title>\r\n</head><body>",
            "<iframe style=\"display:block;border:0;\" src=\"/error",
            "?src=404&amp;ifr=1&amp;error=\" width=\"100%\" height=\"100%\"",
            " frameborder=\"0\" scrolling=\"no\"></iframe></body></html>">>
    },
    [
        {<<"content-type">>, <<"text/html; charset=utf-8">>},
        {<<"content-length">>, <<"303">>},
        {<<"date">>, <<"Tue, 14 Feb 2017 12:38:29 GMT">>},
        {<<"server">>, <<"YouTubeFrontEnd">>},
        {<<"x-xss-protection">>, <<"1; mode=block">>},
        {<<"x-frame-options">>, <<"SAMEORIGIN">>}
    ]).

response_500_subtest() ->
    test_response("test/data/response_500",
    #buoy_resp {
        state = done,
        status_code = 500,
        reason = <<"Internal Server Error">>,
        headers = [
            <<"Server: Cowboy">>,
            <<"Date: Fri, 03 Jun 2016 14:34:26 GMT">>,
            <<"Content-Length: 0">>,
            <<"X-Server: ewr-delivery-6">>
        ],
        content_length = 0
    },
    [
        {<<"Server">>, <<"Cowboy">>},
        {<<"Date">>, <<"Fri, 03 Jun 2016 14:34:26 GMT">>},
        {<<"Content-Length">>, <<"0">>},
        {<<"X-Server">>, <<"ewr-delivery-6">>}
    ]).

response_502_subtest() ->
    test_response("test/data/response_502",
    #buoy_resp {
        state = done,
        status_code = 502,
        reason = <<"Bad Gateway">>,
        headers = [
            <<"Content-Type: application/json">>,
            <<"Content-Length: 108">>
        ],
        content_length = 108,
        body = <<"<html><body><h1>502 Bad Gateway</h1>.The server returned",
            " an invalid or incomplete response\r\n</body></html>\r\n">>
    },
    [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Content-Length">>, <<"108">>}
    ]).

response_custom_subtest() ->
    test_response("test/data/response_custom",
    #buoy_resp {
        state = done,
        status_code = 301,
        reason = <<"MOVED PERMANENTLY">>,
        headers = [
            <<"Server:  ">>,
            <<"Date:  ">>,
            <<"Referer:  ">>,
            <<"Location: http://www.apple.com/">>,
            <<"Content-type: text/html">>,
            <<"Connection: close">>
        ]
    },
    [
        {<<"Server">>, <<" ">>},
        {<<"Date">>, <<" ">>},
        {<<"Referer">>, <<" ">>},
        {<<"Location">>, <<"http://www.apple.com/">>},
        {<<"Content-type">>, <<"text/html">>},
        {<<"Connection">>, <<"close">>}
    ]).

response_http_1_0_subtest() ->
    {ok, [Response]} = file:consult("test/data/response_http_1_0"),

    ?assertEqual({error, unsupported_feature},
        buoy_protocol:response(Response)).

%% private
test_response(Filename, BuoyResp, Headers) ->
    {ok, [Response]} = file:consult(Filename),
    {ok, BuoyResp2, <<>>} = buoy_protocol:response(Response),
    ?assertEqual(BuoyResp, BuoyResp2),
    {ok, Headers2} = buoy_protocol:headers(BuoyResp),
    ?assertEqual(Headers, Headers2).
