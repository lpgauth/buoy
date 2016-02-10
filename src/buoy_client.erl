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
    buffer       = <<>> :: binary(),
    requests_in  = 0    :: non_neg_integer(),
    requests_out = 0    :: non_neg_integer()
}).

-type state() :: #state {}.

%% shackle_server callbacks
-spec init() ->
    {ok, state()}.

init() ->
    {ok, #state {}}.

-spec setup(inet:socket(), state()) ->
    {ok, state()}.

setup(_Socket, State) ->
    {ok, State}.

-spec handle_request(term(), state()) ->
    {ok, non_neg_integer(), iodata(), state()}.

handle_request({request, Request}, #state {
        requests_out = Requests
    } = State) ->

    RequestId = request_id(Requests),
    {ok, RequestId, Request, State#state {
        requests_out = Requests + 1
    }}.

-spec handle_data(binary(), state()) ->
    {ok, [{pos_integer(), term()}], state()}.

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
responses(Data, Requests, State, Responses) ->
    case buoy_protocol:response(Data, undefined) of
        {ok, Response, Rest} ->
            Responses2 = [{Requests, {ok, Response}} | Responses],
            responses(Rest, Requests + 1, State, Responses2);
        {error, not_enough_data} ->
            {ok, Responses, State#state {
                buffer = Data,
                requests_in = Requests
            }};
        {error, Reason} ->
            % TODO: https://github.com/lpgauth/shackle/issues/45
            Responses2 = [{Requests, {error, Reason}} | Responses],
            responses(<<>>, Requests + 1, State, Responses2)
    end.

request_id(N) ->
    N rem ?MAX_32_BIT_INT.
