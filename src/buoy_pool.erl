-module(buoy_pool).
-include("buoy_internal.hrl").

-export([
    init/0,
    lookup/3,
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
    foil:new(?MODULE),
    foil:load(?MODULE).

-spec lookup(protocol_http(), hostname(), inet:port_number()) ->
    {ok, atom()} | {error, pool_not_started}.

lookup(Protocol, Hostname, Port) ->
    case foil:lookup(buoy_pool, {Protocol, Hostname, Port}) of
        {ok, _} = R ->
            R;
        {error, _} ->
            {error, pool_not_started}
    end.

-spec start(buoy_url()) ->
    ok | {error, pool_already_started | shackle_not_started}.

start(Url) ->
    start(Url, ?DEFAULT_POOL_OPTIONS).

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
            Key = {Protocol, Hostname, Port},
            ets:insert(?ETS_TABLE_POOL, {Key, Name}),
            ok = foil:insert(?MODULE, Key, Name),
            ok = foil:load(?MODULE);
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

    Key = {Protocol, Hostname, Port},
    case ets:take(?ETS_TABLE_POOL, Key) of
        [] ->
            {error, pool_not_started};
        [{_, Name}] ->
            foil:delete(?MODULE, Key),
            foil:load(?MODULE),
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
