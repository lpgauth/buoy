-module(buoy_pool).
-include("buoy_internal.hrl").

-ignore_xref([
    {buoy_pool_utils, name, 2}
]).

-export([
    init/0,
    start/2,
    start/3,
    stop/2
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

-spec start(hostname(), inet:port_number()) ->
    ok | {error, pool_already_started | shackle_not_started}.

start(Host, Port) ->
    start(Host, Port, ?DEFAULT_POOL_OTPIONS).

-spec start(hostname(), inet:port_number(), options()) ->
    ok | {error, pool_already_started | shackle_not_started}.

start(Host, Port, Options) ->
    Name = name(Host, Port),
    ClientOptions = client_options(Host, Port, Options),
    PoolOptions = pool_options(Options),
    case shackle_pool:start(Name, ?CLIENT, ClientOptions, PoolOptions) of
        ok ->
            ets:insert(?ETS_TABLE_POOL, {{Host, Port}, Name}),
            buoy_compiler:pool_utils();
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(hostname(), inet:port_number()) ->
    ok | {error, shackle_not_started | pool_not_started}.

stop(Host, Port) ->
    case ets:take(?ETS_TABLE_POOL, {Host, Port}) of
        [] ->
            {error, pool_not_started};
        [{_, PoolName}] ->
            buoy_compiler:pool_utils(),
            shackle_pool:stop(PoolName)
    end.

%% private
name(Host, Port) ->
    list_to_atom(binary_to_list(Host) ++ "_"
        ++ integer_to_list(Port)).

client_options(Host, Port, Options) ->
    Reconnect = ?LOOKUP(reconnect, Options, ?DEFAULT_RECONNECT),
    ReconnectTimeMax = ?LOOKUP(reconnect_time_max, Options,
        ?DEFAULT_RECONNECT_MAX),
    ReconnectTimeMin = ?LOOKUP(reconnect_time_min, Options,
        ?DEFAULT_RECONNECT_MIN),

    [{ip, binary_to_list(Host)},
     {port, Port},
     {protocol, shackle_tcp},
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

pool_options(Options) ->
    BacklogSize = ?LOOKUP(backlog_size, Options, ?DEFAULT_BACKLOG_SIZE),
    PoolSize = ?LOOKUP(pool_size, Options, ?DEFAULT_POOL_SIZE),
    PoolStrategy = ?LOOKUP(pool_strategy, Options, ?DEFAULT_POOL_STRATEGY),

    [{backlog_size, BacklogSize},
     {pool_size, PoolSize},
     {pool_strategy, PoolStrategy}].
