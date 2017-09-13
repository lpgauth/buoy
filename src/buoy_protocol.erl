-module(buoy_protocol).
-include("buoy_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    bin_patterns/0,
    headers/1,
    request/5,
    response/1,
    response/3
]).

-record(bin_patterns, {
    rn   :: binary:cp(),
    rnrn :: binary:cp()
}).

-type bin_patterns() :: #bin_patterns {}.

%% public
-spec bin_patterns() ->
    bin_patterns().

bin_patterns() ->
    #bin_patterns {
        rn   = binary:compile_pattern(<<"\r\n">>),
        rnrn = binary:compile_pattern(<<"\r\n\r\n">>)
    }.

-spec headers(buoy_resp()) ->
    {ok, headers()} | {error, invalid_headers}.

headers(#buoy_resp {headers = Headers}) ->
    parse_headers(Headers, []).

-spec request(method(), path(), headers(), host(), body()) ->
    iolist().

request(Method, Path, Headers, Host, undefined) ->
    [format_method(Method), <<" ">>, Path,
        <<" HTTP/1.1\r\n">>,
        <<"Host: ">>, Host,
        <<"\r\nConnection: Keep-alive\r\n">>,
        <<"User-Agent: buoy\r\n">>,
        format_headers(Headers), <<"\r\n">>];
request(Method, Path, Headers, Host, Body) ->
    ContentLength = integer_to_binary(iolist_size(Body)),
    Headers2 = [{<<"Content-Length">>, ContentLength} | Headers],

    [format_method(Method), <<" ">>, Path,
        <<" HTTP/1.1\r\n">>,
        <<"Host: ">>, Host,
        <<"\r\nConnection: Keep-alive\r\n">>,
        <<"User-Agent: buoy\r\n">>,
        format_headers(Headers2), <<"\r\n">>,
        Body].

-spec response(binary()) ->
    {ok, buoy_resp(), binary()} | error().

response(Data) ->
    response(Data, undefined, bin_patterns()).

-spec response(binary(), undefined | buoy_resp(), bin_patterns()) ->
    {ok, buoy_resp(), binary()} | error().

response(Data, undefined, BinPatterns) ->
    case parse_status_line(Data, BinPatterns) of
        {StatusCode, Reason, Rest} ->
            case split_headers(Rest, BinPatterns) of
                {undefined, Headers, Rest2} ->
                    {ok, #buoy_resp {
                        state = done,
                        status_code = StatusCode,
                        reason = Reason,
                        headers = Headers,
                        content_length = undefined
                    }, Rest2};
                {0, Headers, Rest2} ->
                    {ok, #buoy_resp {
                        state = done,
                        status_code = StatusCode,
                        reason = Reason,
                        headers = Headers,
                        content_length = 0
                    }, Rest2};
                {ContentLength, Headers, Rest2} ->
                    response(Rest2, #buoy_resp {
                        state = body,
                        status_code = StatusCode,
                        reason = Reason,
                        headers = Headers,
                        content_length = ContentLength
                    }, BinPatterns);
                {error, Reason2} ->
                    {error, Reason2}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
response(Data, #buoy_resp {
        state = body,
        content_length = ContentLength
    } = Response, _BinPatterns) when size(Data) >= ContentLength ->

    <<Body:ContentLength/binary, Rest/binary>> = Data,

    {ok, Response#buoy_resp {
        state = done,
        body = Body
    }, Rest};
response(_Data, #buoy_resp {
        state = body
    }, _BinPatterns) ->

    {error, not_enough_data}.

%% private
content_length([]) ->
    {ok, undefined};
content_length([<<"Content-Length: ", Rest/binary>> | _T]) ->
    {ok, binary_to_integer(Rest)};
content_length([<<"content-length: ", Rest/binary>> | _T]) ->
    {ok, binary_to_integer(Rest)};
content_length([<<"Transfer-Encoding: chunked">> | _T]) ->
    {error, unsupported_feature};
content_length([<<"transfer-encoding: chunked">> | _T]) ->
    {error, unsupported_feature};
content_length([_ | T]) ->
    content_length(T).

format_method(get) ->
    <<"GET">>;
format_method(post) ->
    <<"POST">>;
format_method(put) ->
    <<"PUT">>.

format_headers(Headers) ->
    [format_header(Header) || Header <- Headers].

format_header({Key, Value}) ->
    [Key, <<": ">>, Value, <<"\r\n">>].

parse_headers([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_headers([Header | T], Acc) ->
    case binary:split(Header, <<":">>) of
        [Header] ->
            {error, invalid_headers};
        [Key, <<>>] ->
            parse_headers(T, [{Key, undefined} | Acc]);
        [Key, <<" ", Value/binary>>] ->
            parse_headers(T, [{Key, Value} | Acc])
    end.

parse_status_line(Data, #bin_patterns {rn = Rn}) ->
    case binary:split(Data, Rn) of
        [Data] ->
            {error, not_enough_data};
        [Line, Rest] ->
            case parse_status_reason(Line) of
                {ok, StatusCode, Reason} ->
                    {StatusCode, Reason, Rest};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

parse_status_reason(<<"HTTP/1.1 200 OK">>) ->
    {ok, 200, <<"OK">>};
parse_status_reason(<<"HTTP/1.1 204 No Content">>) ->
    {ok, 204, <<"No Content">>};
parse_status_reason(<<"HTTP/1.1 301 Moved Permanently">>) ->
    {ok, 301, <<"Moved Permanently">>};
parse_status_reason(<<"HTTP/1.1 302 Found">>) ->
    {ok, 302, <<"Found">>};
parse_status_reason(<<"HTTP/1.1 403 Forbidden">>) ->
    {ok, 403, <<"Forbidden">>};
parse_status_reason(<<"HTTP/1.1 404 Not Found">>) ->
    {ok, 404, <<"Not Found">>};
parse_status_reason(<<"HTTP/1.1 500 Internal Server Error">>) ->
    {ok, 500, <<"Internal Server Error">>};
parse_status_reason(<<"HTTP/1.1 502 Bad Gateway">>) ->
    {ok, 502, <<"Bad Gateway">>};
parse_status_reason(<<"HTTP/1.1 ", N1, N2, N3, " ", Reason/bits >>)
        when $0 =< N1, N1 =< $9,
             $0 =< N2, N2 =< $9,
             $0 =< N3, N3 =< $9 ->

    StatusCode = (N1 - $0) * 100 + (N2 - $0) * 10 + (N3 - $0),
    {ok, StatusCode, Reason};
parse_status_reason(<<"HTTP/1.0 ", _/binary>>) ->
    {error, unsupported_feature};
parse_status_reason(_) ->
    {error, bad_request}.

split_headers(Data, #bin_patterns {rn = Rn, rnrn = Rnrn}) ->
    case binary:split(Data, Rnrn) of
        [Data] ->
            {error, not_enough_data};
        [Headers, Body] ->
            Headers2 = binary:split(Headers, Rn, [global]),
            case content_length(Headers2) of
                {ok, ContentLength} ->
                    {ContentLength, Headers2, Body};
                {error, Reason} ->
                    {error, Reason}
            end
    end.
