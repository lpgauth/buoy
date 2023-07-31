%% records
-record(buoy_resp, {
    state          :: body | done,
    body           :: undefined | binary(),
    content_length :: undefined | non_neg_integer() | chunked,
    headers        :: undefined | [binary()],
    reason         :: undefined | binary(),
    status_code    :: undefined | 100..505
}).

-record(buoy_url, {
    host     :: host(),
    hostname :: hostname(),
    path     :: path(),
    port     :: inet:port_number(),
    protocol :: protocol_http()
}).

%% types
-type buoy_resp()     :: #buoy_resp {}.
-type buoy_url()      :: #buoy_url {}.
-type error()         :: {error, term()}.
-type host()          :: binary().
-type hostname()      :: binary().
-type method()        :: get | head | post | put | {custom, binary()}.
-type option()        :: {backlog_size, pos_integer()} |
                         {pool_size, pos_integer()} |
                         {pool_strategy, random | round_robin} |
                         {reconnect, boolean()} |
                         {reconnect_time_max, pos_integer() | infinity} |
                         {reconnect_time_min, pos_integer()} |
                         {socket_options, [gen_tcp:connect_option() | ssl:tls_client_option()]}.
-type options()       :: [option()].
-type path()          :: binary().
-type protocol_http() :: http | https.

-export_type([
    buoy_resp/0,
    buoy_url/0
]).
