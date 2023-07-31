-module(buoy).
-include("buoy_internal.hrl").

-compile(inline).
-compile({inline_size, 512}).

-export([
    async_custom/3,
    async_get/2,
    async_head/2,
    async_post/2,
    async_put/2,
    async_request/3,
    custom/3,
    get/2,
    head/2,
    post/2,
    put/2,
    receive_response/1,
    request/3
]).

%% types
-type body() :: undefined | iodata().
-type headers() :: [{iodata(), iodata()}].
-type hostname() :: binary().
-type opts() :: #{headers => headers(),
                  body    => body(),
                  pid     => pid(),
                  timeout => non_neg_integer()}.

-export_type([
    body/0,
    headers/0,
    hostname/0,
    opts/0
]).

%% public
-spec async_custom(binary(), buoy_url(), opts()) ->
    {ok, shackle:request_id()} | error().

async_custom(Verb, Url, BuoyOpts) ->
    async_request({custom, Verb}, Url, BuoyOpts).

-spec async_get(buoy_url(), opts()) ->
    {ok, shackle:request_id()} | error().

async_get(Url, BuoyOpts) ->
    async_request(get, Url, BuoyOpts).

-spec async_head(buoy_url(), opts()) ->
    {ok, shackle:request_id()} | error().

async_head(Url, BuoyOpts) ->
    async_request(head, Url, BuoyOpts).

-spec async_post(buoy_url(), opts()) ->
    {ok, shackle:request_id()} | error().

async_post(Url, BuoyOpts) ->
    async_request(post, Url, BuoyOpts).

-spec async_put(buoy_url(), opts()) ->
    {ok, shackle:request_id()} | error().

async_put(Url, BuoyOpts) ->
    async_request(put, Url, BuoyOpts).

-spec async_request(method(), buoy_url(), opts()) ->
    {ok, shackle:request_id()} | error().

async_request(Method, #buoy_url {
        protocol = Protocol,
        host = Host,
        hostname = Hostname,
        port = Port,
        path = Path
    }, BuoyOpts) ->

    case buoy_pool:lookup(Protocol, Hostname, Port) of
        {ok, PoolName} ->
            Headers = buoy_opts(headers, BuoyOpts),
            Body = buoy_opts(body, BuoyOpts),
            Request = {request, Method, Path, Headers, Host, Body},
            Pid = buoy_opts(pid, BuoyOpts),
            Timeout = buoy_opts(timeout, BuoyOpts),
            shackle:cast(PoolName, Request, Pid, Timeout);
        {error, _} = E ->
            E
    end.

-spec custom(binary(), buoy_url(), opts()) ->
    {ok, buoy_resp()} | error().

custom(Verb, Url, BuoyOpts) ->
    request({custom, Verb}, Url, BuoyOpts).

-spec get(buoy_url(), opts()) ->
    {ok, buoy_resp()} | error().

get(Url, BuoyOpts) ->
    request(get, Url, BuoyOpts).

-spec head(buoy_url(), opts()) ->
    {ok, buoy_resp()} | error().

head(Url, BuoyOpts) ->
    request(head, Url, BuoyOpts).

-spec post(buoy_url(), opts()) ->
    {ok, buoy_resp()} | error().

post(Url, BuoyOpts) ->
    request(post, Url, BuoyOpts).

-spec put(buoy_url(), opts()) ->
    {ok, buoy_resp()} | error().

put(Url, BuoyOpts) ->
    request(put, Url, BuoyOpts).

-spec receive_response(shackle:request_id()) ->
    {ok, term()} | error().

receive_response(RequestId) ->
    shackle:receive_response(RequestId).

-spec request(method(), buoy_url(), opts()) ->
    {ok, buoy_resp()} | error().

request(Method, #buoy_url {
        protocol = Protocol,
        host = Host,
        hostname = Hostname,
        port = Port,
        path = Path
    }, BuoyOpts) ->

    case buoy_pool:lookup(Protocol, Hostname, Port) of
        {ok, PoolName} ->
            Headers = buoy_opts(headers, BuoyOpts),
            Body = buoy_opts(body, BuoyOpts),
            Request = {request, Method, Path, Headers, Host, Body},
            Timeout = buoy_opts(timeout, BuoyOpts),
            shackle:call(PoolName, Request, Timeout);
        {error, _} = E ->
            E
    end.

%% private
buoy_opts(body, BuoyOpts) ->
    maps:get(body, BuoyOpts, ?DEFAULT_BODY);
buoy_opts(headers, BuoyOpts) ->
    maps:get(headers, BuoyOpts, ?DEFAULT_HEADERS);
buoy_opts(pid, BuoyOpts) ->
    maps:get(pid, BuoyOpts, ?DEFAULT_PID);
buoy_opts(timeout, BuoyOpts) ->
    maps:get(timeout, BuoyOpts, ?DEFAULT_TIMEOUT).
