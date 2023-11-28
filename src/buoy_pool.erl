-module(buoy_pool).
-include("buoy.hrl").
-include("buoy_internal.hrl").

-export([
    init/0,
    lookup/3,
    start/1,
    start/2,
    stop/1,
    terminate/0
]).

%% types
-type option() :: {backlog_size, pos_integer()} |
                  {pool_size, pos_integer()} |
                  {pool_strategy, random | round_robin} |
                  {reconnect, boolean()} |
                  {reconnect_time_max, pos_integer() | infinity} |
                  {reconnect_time_min, pos_integer()} |
                  {socket_options, [gen_tcp:connect_option() | ssl:tls_client_option()]}.
-type options() :: [option()].
-type protocol() :: http | https.

-export_type([
    options/0,
    protocol/0
]).

%% public
-spec init() ->
    ok.

init() ->
    foil:new(?MODULE),
    foil:load(?MODULE).

-spec lookup(protocol(), buoy:hostname(), inet:port_number()) ->
    {ok, atom()} | {error, pool_not_started | buoy_not_started}.

lookup(Protocol, Hostname, Port) ->
    case foil:lookup(buoy_pool, {Protocol, Hostname, Port}) of
        {ok, _} = R ->
            R;
        {error, key_not_found} ->
            {error, pool_not_started};
        {error, _} ->
            {error, buoy_not_started}
    end.

-spec start(buoy:url()) ->
    ok | {error, pool_already_started | buoy_not_started}.

start(Url) ->
    start(Url, ?DEFAULT_POOL_OPTIONS).

-spec start(buoy:url(), options()) ->
    ok | {error, pool_already_started | buoy_not_started}.

start(#buoy_url {
        protocol = Protocol,
        hostname = Hostname,
        port = Port
    }, Options) ->

    Name = name(Protocol, Hostname, Port),
    ClientOptions = client_options(Protocol, Hostname, Port, Options),
    PoolOptions = pool_options(Options),

    case shackle_pool:start(Name, ?CLIENT, ClientOptions, PoolOptions) of
        ok ->
            Key = {Protocol, Hostname, Port},
            ok = foil:insert(?MODULE, Key, Name),
            ok = foil:load(?MODULE);
        {error, pool_already_started} = E ->
            E;
        {error, shackle_not_started} ->
            {error, buoy_not_started}
    end.

-spec stop(buoy:url()) ->
    ok | {error,  pool_not_started | buoy_not_started}.

stop(#buoy_url {
        protocol = Protocol,
        hostname = Hostname,
        port = Port
    }) ->

    Key = {Protocol, Hostname, Port},
    case foil:lookup(?MODULE, Key) of
        {ok, Name} ->
            shackle_pool:stop(Name),
            foil:delete(?MODULE, Key),
            foil:load(?MODULE);
        {error, key_not_found} ->
            {error, pool_not_started};
        {error, _} ->
            {error, buoy_not_started}
    end.

-spec terminate() ->
    ok.

terminate() ->
    foil:delete(?MODULE).

%% private
client_options(Protocol, Hostname, Port, Options) ->
    Reconnect = ?LOOKUP(reconnect, Options, ?DEFAULT_RECONNECT),
    ReconnectTimeMax = ?LOOKUP(reconnect_time_max, Options,
        ?DEFAULT_RECONNECT_MAX),
    ReconnectTimeMin = ?LOOKUP(reconnect_time_min, Options,
        ?DEFAULT_RECONNECT_MIN),
    SocketOptions = ?LOOKUP(socket_options, Options,
        ?DEFAULT_SOCKET_OPTIONS),

    [{ip, binary_to_list(Hostname)},
     {port, Port},
     {protocol, shackle_protocol(Protocol)},
     {reconnect, Reconnect},
     {reconnect_time_max, ReconnectTimeMax},
     {reconnect_time_min, ReconnectTimeMin},
     {socket_options, SocketOptions}].

name(Protocol, Hostname, Port) ->
    list_to_atom(atom_to_list(Protocol) ++ "_"
        ++ binary_to_list(Hostname) ++ "_"
        ++ integer_to_list(Port)).

pool_options(Options) ->
    BacklogSize = ?LOOKUP(backlog_size, Options, ?DEFAULT_BACKLOG_SIZE),
    PoolSize = ?LOOKUP(pool_size, Options, ?DEFAULT_POOL_SIZE),
    PoolStrategy = ?LOOKUP(pool_strategy, Options, ?DEFAULT_POOL_STRATEGY),

    [{backlog_size, BacklogSize},
     {pool_size, PoolSize},
     {pool_strategy, PoolStrategy}].

shackle_protocol(http) ->
    shackle_tcp;
shackle_protocol(https) ->
    shackle_ssl.
