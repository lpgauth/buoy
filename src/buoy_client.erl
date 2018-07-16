-module(buoy_client).
-include("buoy_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-behavior(shackle_client).
-export([
    init/1,
    setup/2,
    handle_request/2,
    handle_data/2,
    terminate/1
]).

-record(state, {
    bin_patterns        :: tuple(),
    buffer       = <<>> :: binary(),
    requests_in  = 0    :: non_neg_integer(),
    requests_out = 0    :: non_neg_integer(),
    response            :: undefined | buoy_resp()
}).

-type state() :: #state {}.

%% shackle_server callbacks
-spec init(undefined) ->
    {ok, state()}.

init(_Opts) ->
    {ok, #state {
        bin_patterns = buoy_protocol:bin_patterns()
    }}.

-spec setup(inet:socket(), state()) ->
    {ok, state()}.

setup(_Socket, State) ->
    {ok, State}.

-spec handle_request(term(), state()) ->
    {ok, non_neg_integer(), iodata(), state()}.

handle_request({request, Method, Path, Headers, Host, Body}, #state {
        requests_out = RequestsOut
    } = State) ->

    Request = buoy_protocol:request(Method, Path, Headers, Host, Body),
    {ok, RequestsOut, Request, State#state {
        requests_out = RequestsOut + 1
    }}.

-spec handle_data(binary(), state()) ->
    {ok, [{pos_integer(), term()}], state()} |
    {error, atom(), state()}.

handle_data(Data, #state {
        bin_patterns = BinPatterns,
        buffer = Buffer,
        requests_in = RequestsIn,
        response = Response
    } = State) ->

    Data2 = <<Buffer/binary, Data/binary>>,
    case responses(Data2, RequestsIn, Response, BinPatterns, []) of
        {ok, RequestsIn2, Response2, Responses, Rest} ->
            {ok, Responses, State#state {
                buffer = Rest,
                requests_in = RequestsIn2,
                response = Response2
            }};
        {error, Reason} ->
            {error, Reason, State}
    end.

-spec terminate(state()) ->
    ok.

terminate(_State) ->
    ok.

%% private
responses(<<>>, RequestsIn, Response, _BinPatterns, Responses) ->
    {ok, RequestsIn, Response, Responses, <<>>};
responses(Data, RequestsIn, Response, BinPatterns, Responses) ->
    case buoy_protocol:response(Data, Response, BinPatterns) of
        {ok, #buoy_resp {state = done} = Response2, Rest} ->
            Responses2 = [{RequestsIn, {ok, Response2}} | Responses],
            responses(Rest, RequestsIn + 1, undefined, BinPatterns, Responses2);
        {ok, #buoy_resp {} = Response2, Rest} ->
            {ok, RequestsIn, Response2, Responses, Rest};
        {error, not_enough_data} ->
            {ok, RequestsIn, Response, Responses, Data};
        {error, _Reason} = E ->
            E
    end.
