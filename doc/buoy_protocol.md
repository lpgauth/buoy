

# Module buoy_protocol #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-backlog_size">backlog_size()</a> ###


<pre><code>
backlog_size() = pos_integer() | infinity
</code></pre>




### <a name="type-bin_patterns">bin_patterns()</a> ###


<pre><code>
bin_patterns() = #bin_patterns{rn = <a href="binary.md#type-cp">binary:cp()</a>, rnrn = <a href="binary.md#type-cp">binary:cp()</a>}
</code></pre>




### <a name="type-body">body()</a> ###


<pre><code>
body() = undefined | iodata()
</code></pre>




### <a name="type-buoy_resp">buoy_resp()</a> ###


<pre><code>
buoy_resp() = #buoy_resp{state = body | done, body = undefined | binary(), content_length = undefined | non_neg_integer() | chunked, headers = undefined | [binary()], reason = undefined | binary(), status_code = undefined | 100..505}
</code></pre>




### <a name="type-buoy_url">buoy_url()</a> ###


<pre><code>
buoy_url() = #buoy_url{host = <a href="#type-host">host()</a>, hostname = <a href="#type-hostname">hostname()</a>, path = <a href="#type-path">path()</a>, port = <a href="inet.md#type-port_number">inet:port_number()</a>, protocol = <a href="#type-protocol_http">protocol_http()</a>}
</code></pre>




### <a name="type-client_option">client_option()</a> ###


<pre><code>
client_option() = {ip, <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a>} | {protocol, <a href="#type-protocol">protocol()</a>} | {reconnect, boolean()} | {reconnect_time_max, <a href="#type-time">time()</a> | infinity} | {reconnect_time_min, <a href="#type-time">time()</a>} | {socket_options, [<a href="gen_tcp.md#type-connect_option">gen_tcp:connect_option()</a> | <a href="gen_udp.md#type-option">gen_udp:option()</a>]}
</code></pre>




### <a name="type-client_options">client_options()</a> ###


<pre><code>
client_options() = [<a href="#type-client_option">client_option()</a>]
</code></pre>




### <a name="type-error">error()</a> ###


<pre><code>
error() = {error, term()}
</code></pre>




### <a name="type-headers">headers()</a> ###


<pre><code>
headers() = [{iodata(), iodata()}]
</code></pre>




### <a name="type-host">host()</a> ###


<pre><code>
host() = binary()
</code></pre>




### <a name="type-hostname">hostname()</a> ###


<pre><code>
hostname() = binary()
</code></pre>




### <a name="type-method">method()</a> ###


<pre><code>
method() = get | post
</code></pre>




### <a name="type-path">path()</a> ###


<pre><code>
path() = binary()
</code></pre>




### <a name="type-pool_option">pool_option()</a> ###


<pre><code>
pool_option() = {backlog_size, <a href="#type-backlog_size">backlog_size()</a>} | {pool_size, <a href="#type-pool_size">pool_size()</a>} | {pool_strategy, <a href="#type-pool_strategy">pool_strategy()</a>}
</code></pre>




### <a name="type-pool_options">pool_options()</a> ###


<pre><code>
pool_options() = [<a href="#type-pool_option">pool_option()</a>]
</code></pre>




### <a name="type-pool_size">pool_size()</a> ###


<pre><code>
pool_size() = pos_integer()
</code></pre>




### <a name="type-pool_strategy">pool_strategy()</a> ###


<pre><code>
pool_strategy() = random | round_robin
</code></pre>




### <a name="type-protocol">protocol()</a> ###


<pre><code>
protocol() = shackle_ssl | shackle_tcp | shackle_udp
</code></pre>




### <a name="type-protocol_http">protocol_http()</a> ###


<pre><code>
protocol_http() = http | https
</code></pre>




### <a name="type-time">time()</a> ###


<pre><code>
time() = pos_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bin_patterns-0">bin_patterns/0</a></td><td></td></tr><tr><td valign="top"><a href="#headers-1">headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#request-5">request/5</a></td><td></td></tr><tr><td valign="top"><a href="#response-1">response/1</a></td><td></td></tr><tr><td valign="top"><a href="#response-3">response/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bin_patterns-0"></a>

### bin_patterns/0 ###

<pre><code>
bin_patterns() -&gt; <a href="#type-bin_patterns">bin_patterns()</a>
</code></pre>
<br />

<a name="headers-1"></a>

### headers/1 ###

<pre><code>
headers(Buoy_resp::<a href="#type-buoy_resp">buoy_resp()</a>) -&gt; {ok, <a href="#type-headers">headers()</a>} | {error, invalid_headers}
</code></pre>
<br />

<a name="request-5"></a>

### request/5 ###

<pre><code>
request(Method::<a href="#type-method">method()</a>, Path::<a href="#type-path">path()</a>, Headers::<a href="#type-headers">headers()</a>, Host::<a href="#type-host">host()</a>, Body::<a href="#type-body">body()</a>) -&gt; iolist()
</code></pre>
<br />

<a name="response-1"></a>

### response/1 ###

<pre><code>
response(Data::binary()) -&gt; {ok, <a href="#type-buoy_resp">buoy_resp()</a>, binary()} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="response-3"></a>

### response/3 ###

<pre><code>
response(Data::binary(), Buoy_resp::undefined | <a href="#type-buoy_resp">buoy_resp()</a>, BinPatterns::<a href="#type-bin_patterns">bin_patterns()</a>) -&gt; {ok, <a href="#type-buoy_resp">buoy_resp()</a>, binary()} | <a href="#type-error">error()</a>
</code></pre>
<br />

