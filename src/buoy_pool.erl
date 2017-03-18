-module(buoy_pool).
-include("buoy_internal.hrl").

-export([
    init/0,
    start/1,
    start/2,
    stop/1
]).

%% public
-spec init() ->
    ok.

init() ->
    ets:new(?ETS_TABLE_POOL, [
        named_table,
        public
    ]),
    buoy_compiler:pool_utils().

-spec start(buoy_url()) ->
    ok | {error, pool_already_started | shackle_not_started}.

start(Url) ->
    start(Url, ?DEFAULT_POOL_OTPIONS).

-spec start(buoy_url(), options()) ->
    ok | {error, pool_already_started | shackle_not_started}.

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
            ets:insert(?ETS_TABLE_POOL, {{Protocol, Hostname, Port}, Name}),
            buoy_compiler:pool_utils();
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(buoy_url()) ->
    ok | {error, shackle_not_started | pool_not_started}.

stop(#buoy_url {
        protocol = Protocol,
        hostname = Hostname,
        port = Port
    }) ->

    case ets:take(?ETS_TABLE_POOL, {Protocol, Hostname, Port}) of
        [] ->
            {error, pool_not_started};
        [{_, Name}] ->
            buoy_compiler:pool_utils(),
            shackle_pool:stop(Name)
    end.

%% private
client_options(Protocol, Hostname, Port, Options) ->
    Reconnect = ?LOOKUP(reconnect, Options, ?DEFAULT_RECONNECT),
    ReconnectTimeMax = ?LOOKUP(reconnect_time_max, Options,
        ?DEFAULT_RECONNECT_MAX),
    ReconnectTimeMin = ?LOOKUP(reconnect_time_min, Options,
        ?DEFAULT_RECONNECT_MIN),

    [{ip, binary_to_list(Hostname)},
     {port, Port},
     {protocol, shackle_protocol(Protocol)},
     {reconnect, Reconnect},
     {reconnect_time_max, ReconnectTimeMax},
     {reconnect_time_min, ReconnectTimeMin},
     {socket_options, [
         binary,
         {packet, line},
         {packet, raw},
         {send_timeout, 50},
         {send_timeout_close, true}
     ]}].

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
