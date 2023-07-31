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
    protocol :: buoy_pool:protocol()
}).
