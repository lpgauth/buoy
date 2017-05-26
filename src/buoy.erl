-module(buoy).
-include("buoy_internal.hrl").

-ignore_xref([
    {buoy_pool_utils, name, 3}
]).

-compile(inline).
-compile({inline_size, 512}).

-export([
    async_get/1,
    async_get/2,
    async_get/3,
    async_get/4,
    async_post/1,
    async_post/2,
    async_post/3,
    async_post/4,
    async_request/6,
    get/1,
    get/2,
    get/3,
    post/1,
    post/2,
    post/3,
    post/4,
    receive_response/1,
    request/5
]).

%% public
-spec async_get(buoy_url()) ->
    {ok, shackle:request_id()} | error().

async_get(Url) ->
    async_get(Url, ?DEFAULT_HEADERS).

-spec async_get(buoy_url(), headers()) ->
    {ok, shackle:request_id()} | error().

async_get(Url, Headers) ->
    async_get(Url, Headers, self()).

-spec async_get(buoy_url(), headers(), pid()) ->
    {ok, shackle:request_id()} | error().

async_get(Url, Headers, Pid) ->
    async_get(Url, Headers, Pid, ?DEFAULT_TIMEOUT).

-spec async_get(buoy_url(), headers(), pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_get(Url, Headers, Pid, Timeout) ->
    async_request(get, Url, Headers, ?DEFAULT_BODY, Pid, Timeout).

-spec async_post(buoy_url()) ->
    {ok, shackle:request_id()} | error().

async_post(Url) ->
    async_post(Url, ?DEFAULT_HEADERS).

-spec async_post(buoy_url(), headers()) ->
    {ok, shackle:request_id()} | error().

async_post(Url, Headers) ->
    async_post(Url, Headers, ?DEFAULT_BODY).

-spec async_post(buoy_url(), headers(), body()) ->
    {ok, shackle:request_id()} | error().

async_post(Url, Headers, Body) ->
    async_post(Url, Headers, Body, self()).

-spec async_post(buoy_url(), headers(), body(), pid()) ->
    {ok, shackle:request_id()} | error().

async_post(Url, Headers, Body, Pid) ->
    async_post(Url, Headers, Body, Pid, ?DEFAULT_TIMEOUT).

-spec async_post(buoy_url(), headers(), body(), pid(), timeout()) ->
    {ok, shackle:request_id()} | error().

async_post(Url, Headers, Body, Pid, Timeout) ->
    async_request(post, Url, Headers, Body, Pid, Timeout).

-spec async_request(method(), buoy_url(), headers(), body(),
    pid(), timeout()) -> {ok, shackle:request_id()} | error().

async_request(Method, #buoy_url {
        protocol = Protocol,
        host = Host,
        hostname = Hostname,
        port = Port,
        path = Path
    }, Headers, Body, Pid, Timeout) ->

    PoolName = buoy_pool_utils:name(Protocol, Hostname, Port),
    cast(PoolName, {request, Method, Path, Headers, Host, Body}, Pid, Timeout).

-spec get(buoy_url()) ->
    {ok, buoy_resp()} | error().

get(Url) ->
    get(Url, ?DEFAULT_HEADERS).

-spec get(buoy_url(), headers()) ->
    {ok, buoy_resp()} | error().

get(Url, Headers) ->
    get(Url, Headers, ?DEFAULT_TIMEOUT).

-spec get(buoy_url(), headers(), timeout()) ->
    {ok, buoy_resp()} | error().

get(Url, Headers, Timeout) ->
    request(get, Url, Headers, ?DEFAULT_BODY, Timeout).

-spec post(buoy_url()) ->
    {ok, buoy_resp()} | error().

post(Url) ->
    post(Url, ?DEFAULT_HEADERS).

-spec post(buoy_url(), headers()) ->
    {ok, buoy_resp()} | error().

post(Url, Headers) ->
    post(Url, Headers, ?DEFAULT_BODY).

-spec post(buoy_url(), headers(), body()) ->
    {ok, buoy_resp()} | error().

post(Url, Headers, Body) ->
    post(Url, Headers, Body, ?DEFAULT_TIMEOUT).

-spec post(buoy_url(), headers(), body(), timeout()) ->
    {ok, buoy_resp()} | error().

post(Url, Headers, Body, Timeout) ->
    request(post, Url, Headers, Body, Timeout).

-spec receive_response(request_id()) ->
    {ok, term()} | error().

receive_response(RequestId) ->
    shackle:receive_response(RequestId).

-spec request(method(), buoy_url(), headers(), body(), timeout()) ->
    {ok, buoy_resp()} | error().

request(Method, #buoy_url {
        protocol = Protocol,
        host = Host,
        hostname = Hostname,
        port = Port,
        path = Path
    }, Headers, Body, Timeout) ->

    PoolName = buoy_pool_utils:name(Protocol, Hostname, Port),
    call(PoolName, {request, Method, Path, Headers, Host, Body}, Timeout).

%% private
cast(undefined, _Request, _Pid, _Timeout) ->
    {error, pool_not_started};
cast(PoolName, Request, Pid, Timeout) ->
    shackle:cast(PoolName, Request, Pid, Timeout).

call(undefined, _Request, _Timeout) ->
    {error, pool_not_started};
call(PoolName, Request, Timeout) ->
    shackle:call(PoolName, Request, Timeout).
