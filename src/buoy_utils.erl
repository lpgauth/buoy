-module(buoy_utils).
-include("buoy.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    parse_url/1
]).

%% public
-spec parse_url(binary()) ->
    buoy:url() | {error, invalid_url}.

parse_url(<<"http://", Rest/binary>>) ->
    parse_url(http, Rest);
parse_url(<<"https://", Rest/binary>>) ->
    parse_url(https, Rest);
parse_url(_) ->
    {error, invalid_url}.

%% private
parse_url(Protocol, Rest) ->
    {Host, Path} = case binary:split(Rest, <<"/">>, [trim]) of
        [Host2] ->
            {Host2, <<"/">>};
        [Host2, Path2] ->
            {Host2, <<"/", Path2/binary>>}
    end,

    {Hostname, Port} = case binary:split(Host, <<":">>, [trim]) of
        [Host] ->
            case Protocol of
                http ->
                    {Host, 80};
                https ->
                    {Host, 443}
            end;
        [Hostname2, Port2] ->
            {Hostname2, binary_to_integer(Port2)}
    end,

    #buoy_url {
        host = Host,
        hostname = Hostname,
        path = Path,
        port = Port,
        protocol = Protocol
    }.
