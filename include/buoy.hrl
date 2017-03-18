%% records
-record(buoy_resp, {
    state          :: body | done,
    body           :: undefined | binary(),
    content_length :: undefined | non_neg_integer(),
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
-type body()          :: undefined | iodata().
-type buoy_resp()     :: #buoy_resp {}.
-type buoy_url()      :: #buoy_url {}.
-type error()         :: {error, term()}.
-type headers()       :: [{iodata(), iodata()}].
-type host()          :: binary().
-type hostname()      :: binary().
-type method()        :: get | post.
-type option()        :: backlog_size |
                         pool_size |
                         pool_strategy |
                         reconnect |
                         reconnect_time_max |
                         reconnect_time_min.
-type options()       :: [option()].
-type path()          :: binary().
-type protocol_http() :: http | https.

-export_type([
    buoy_resp/0,
    buoy_url/0
]).
