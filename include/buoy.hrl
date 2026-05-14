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
-type body()          :: undefined | iodata().
-type buoy_opts()     :: #{headers => headers(),
                           body    => body(),
                           pid     => pid(),
                           timeout => non_neg_integer()}.
-type buoy_resp()     :: #buoy_resp {}.
-type buoy_url()      :: #buoy_url {}.
%% Documentation type: the actual atoms buoy can put in the second
%% slot of {error, _}. error/0 stays loose ({error, term()}) because
%% buoy_protocol's parser narrows the success-typing of its callees
%% in a way that produces dialyzer false positives if error/0 itself
%% is a closed sum -- see buoy_client:responses/5 where the
%% {error, not_enough_data} buffering pattern is semantically
%% required at runtime but is unreachable from dialyzer's flow
%% analysis when error/0 is tightened.
-type error_reason()  :: %% buoy-level errors
                         pool_not_started | buoy_not_started |
                         pool_already_started | invalid_url |
                         %% HTTP parser errors from buoy_protocol
                         invalid_headers | invalid_chunk_size |
                         %% shackle errors that propagate through buoy
                         no_server | shackle_not_started | timeout.
-type error()         :: {error, term()}.
-type headers()       :: [{iodata(), iodata()}].
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
    buoy_url/0,
    error/0,
    error_reason/0
]).
