-module(buoy_client).
-include("buoy_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-behavior(shackle_client).
-export([
    init/0,
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
-spec init() ->
    {ok, state()}.

init() ->
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
        requests_out = Requests
    } = State) ->

    Request = buoy_protocol:request(Method, Path, Headers, Host, Body),
    {ok, Requests, Request, State#state {
        requests_out = Requests + 1
    }}.

-spec handle_data(binary(), state()) ->
    {ok, [{pos_integer(), term()}], state()} |
    {error, atom(), state()}.

handle_data(Data, #state {
        buffer = Buffer,
        requests_in = Requests
    } = State) ->

    Data2 = <<Buffer/binary, Data/binary>>,
    responses(Data2, Requests, State, []).

-spec terminate(state()) ->
    ok.

terminate(_State) ->
    ok.

%% private
responses(<<>>, Requests, State, Responses) ->
    {ok, Responses, State#state {
        buffer = <<>>,
        requests_in = Requests
    }};
responses(Data, Requests, #state {
        bin_patterns = BinPatterns,
        response = Response
    } = State, Responses) ->

    case buoy_protocol:response(Data, Response, BinPatterns) of
        {ok, #buoy_resp {state = done} = Response2, Rest} ->
            Responses2 = [{Requests, {ok, Response2}} | Responses],
            responses(Rest, Requests + 1, State, Responses2);
        {ok, #buoy_resp {} = Response2, Rest} ->
            {ok, Responses, State#state {
                buffer = Rest,
                requests_in = Requests,
                response = Response2
            }};
        {error, not_enough_data} ->
            {ok, Responses, State#state {
                buffer = Data,
                requests_in = Requests
            }};
        {error, Reason} ->
            {error, Reason, State}
    end.
