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
    host     :: buoy:host(),
    hostname :: buoy:hostname(),
    path     :: buoy:path(),
    port     :: inet:port_number(),
    protocol :: protocol_http()
}).

%% types
-type buoy_resp()     :: #buoy_resp {}.
-type buoy_url()      :: #buoy_url {}.
-type error()         :: {error, term()}.
-type protocol_http() :: http | https.

-export_type([
    buoy_resp/0,
    buoy_url/0
]).
