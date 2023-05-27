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
    queue               :: queue:queue(),
    request_id   = 0    :: non_neg_integer(),
    response            :: undefined | buoy_resp()
}).

-type state() :: #state {}.

%% shackle_server callbacks
-spec init(undefined) ->
    {ok, state()}.

init(_Opts) ->
    {ok, #state {
        bin_patterns = buoy_protocol:bin_patterns(),
        queue = queue:new()
    }}.

-spec setup(inet:socket(), state()) ->
    {ok, state()}.

setup(_Socket, State) ->
    {ok, State}.

-spec handle_request(term(), state()) ->
    {ok, non_neg_integer(), iodata(), state()}.

handle_request({request, Method, Path, Headers, Host, Body}, #state {
        queue = Queue,
        request_id = RequestId
    } = State) ->

    Request = buoy_protocol:request(Method, Path, Headers, Host, Body),

    {ok, RequestId, Request, State#state {
        queue = queue:in({RequestId, Method}, Queue),
        request_id = RequestId + 1
    }}.

-spec handle_data(binary(), state()) ->
    {ok, [{pos_integer(), term()}], state()} |
    {error, atom(), state()}.

handle_data(Data, #state {
        bin_patterns = BinPatterns,
        buffer = Buffer,
        queue = Queue,
        response = Response
    } = State) ->

    Data2 = <<Buffer/binary, Data/binary>>,
    case responses(Data2, Queue, Response, BinPatterns, []) of
        {ok, Queue2, Response2, Responses, Rest} ->
            {ok, Responses, State#state {
                buffer = Rest,
                queue = Queue2,
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
responses(<<>>, Queue, Response, _BinPatterns, Responses) ->
    {ok, Queue, Response, Responses, <<>>};
responses(Data, Queue, Response, BinPatterns, Responses) ->
    {value, {_, Method}} = queue:peek(Queue),
    case buoy_protocol:response(Data, Method, Response, BinPatterns) of
        {ok, #buoy_resp {state = done} = Response2, Rest} ->
            {{value, {RequestId, _}}, Queue2} = queue:out(Queue),
            Responses2 = [{RequestId, {ok, Response2}} | Responses],
            responses(Rest, Queue2, undefined, BinPatterns, Responses2);
        {ok, #buoy_resp {} = Response2, Rest} ->
            {ok, Queue, Response2, Responses, Rest};
        {error, not_enough_data} ->
            {ok, Queue, Response, Responses, Data};
        {error, _Reason} = E ->
            E
    end.
