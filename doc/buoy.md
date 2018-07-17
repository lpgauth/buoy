

# Module buoy #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-backlog_size">backlog_size()</a> ###


<pre><code>
backlog_size() = pos_integer() | infinity
</code></pre>




### <a name="type-body">body()</a> ###


<pre><code>
body() = undefined | iodata()
</code></pre>




### <a name="type-buoy_opts">buoy_opts()</a> ###


<pre><code>
buoy_opts() = #{headers =&gt; <a href="#type-headers">headers()</a>, body =&gt; <a href="#type-body">body()</a>, pid =&gt; pid(), timeout =&gt; non_neg_integer()}
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
client_option() = {init_options, <a href="#type-init_options">init_options()</a>} | {ip, <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a>} | {protocol, <a href="#type-protocol">protocol()</a>} | {reconnect, boolean()} | {reconnect_time_max, <a href="#type-time">time()</a> | infinity} | {reconnect_time_min, <a href="#type-time">time()</a>} | {socket_options, [<a href="gen_tcp.md#type-connect_option">gen_tcp:connect_option()</a> | <a href="gen_udp.md#type-option">gen_udp:option()</a>]}
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




### <a name="type-init_options">init_options()</a> ###


<pre><code>
init_options() = term()
</code></pre>




### <a name="type-method">method()</a> ###


<pre><code>
method() = get | post | put | {custom, binary()}
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




### <a name="type-request_id">request_id()</a> ###


<pre><code>
request_id() = {<a href="#type-server_name">server_name()</a>, reference()}
</code></pre>




### <a name="type-server_name">server_name()</a> ###


<pre><code>
server_name() = atom()
</code></pre>




### <a name="type-time">time()</a> ###


<pre><code>
time() = pos_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_custom-3">async_custom/3</a></td><td></td></tr><tr><td valign="top"><a href="#async_get-2">async_get/2</a></td><td></td></tr><tr><td valign="top"><a href="#async_post-2">async_post/2</a></td><td></td></tr><tr><td valign="top"><a href="#async_put-2">async_put/2</a></td><td></td></tr><tr><td valign="top"><a href="#async_request-3">async_request/3</a></td><td></td></tr><tr><td valign="top"><a href="#custom-3">custom/3</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td></td></tr><tr><td valign="top"><a href="#post-2">post/2</a></td><td></td></tr><tr><td valign="top"><a href="#put-2">put/2</a></td><td></td></tr><tr><td valign="top"><a href="#receive_response-1">receive_response/1</a></td><td></td></tr><tr><td valign="top"><a href="#request-3">request/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_custom-3"></a>

### async_custom/3 ###

<pre><code>
async_custom(Verb::binary(), Url::<a href="#type-buoy_url">buoy_url()</a>, BuoyOpts::<a href="#type-buoy_opts">buoy_opts()</a>) -&gt; {ok, <a href="/Users/lp.gauthier/Git/buoy/_build/default/lib/shackle/doc/shackle.md#type-request_id">shackle:request_id()</a>} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="async_get-2"></a>

### async_get/2 ###

<pre><code>
async_get(Url::<a href="#type-buoy_url">buoy_url()</a>, BuoyOpts::<a href="#type-buoy_opts">buoy_opts()</a>) -&gt; {ok, <a href="/Users/lp.gauthier/Git/buoy/_build/default/lib/shackle/doc/shackle.md#type-request_id">shackle:request_id()</a>} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="async_post-2"></a>

### async_post/2 ###

<pre><code>
async_post(Url::<a href="#type-buoy_url">buoy_url()</a>, BuoyOpts::<a href="#type-buoy_opts">buoy_opts()</a>) -&gt; {ok, <a href="/Users/lp.gauthier/Git/buoy/_build/default/lib/shackle/doc/shackle.md#type-request_id">shackle:request_id()</a>} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="async_put-2"></a>

### async_put/2 ###

<pre><code>
async_put(Url::<a href="#type-buoy_url">buoy_url()</a>, BuoyOpts::<a href="#type-buoy_opts">buoy_opts()</a>) -&gt; {ok, <a href="/Users/lp.gauthier/Git/buoy/_build/default/lib/shackle/doc/shackle.md#type-request_id">shackle:request_id()</a>} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="async_request-3"></a>

### async_request/3 ###

<pre><code>
async_request(Method::<a href="#type-method">method()</a>, Buoy_url::<a href="#type-buoy_url">buoy_url()</a>, BuoyOpts::<a href="#type-buoy_opts">buoy_opts()</a>) -&gt; {ok, <a href="/Users/lp.gauthier/Git/buoy/_build/default/lib/shackle/doc/shackle.md#type-request_id">shackle:request_id()</a>} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="custom-3"></a>

### custom/3 ###

<pre><code>
custom(Verb::binary(), Url::<a href="#type-buoy_url">buoy_url()</a>, BuoyOpts::<a href="#type-buoy_opts">buoy_opts()</a>) -&gt; {ok, <a href="#type-buoy_resp">buoy_resp()</a>} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="get-2"></a>

### get/2 ###

<pre><code>
get(Url::<a href="#type-buoy_url">buoy_url()</a>, BuoyOpts::<a href="#type-buoy_opts">buoy_opts()</a>) -&gt; {ok, <a href="#type-buoy_resp">buoy_resp()</a>} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="post-2"></a>

### post/2 ###

<pre><code>
post(Url::<a href="#type-buoy_url">buoy_url()</a>, BuoyOpts::<a href="#type-buoy_opts">buoy_opts()</a>) -&gt; {ok, <a href="#type-buoy_resp">buoy_resp()</a>} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="put-2"></a>

### put/2 ###

<pre><code>
put(Url::<a href="#type-buoy_url">buoy_url()</a>, BuoyOpts::<a href="#type-buoy_opts">buoy_opts()</a>) -&gt; {ok, <a href="#type-buoy_resp">buoy_resp()</a>} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="receive_response-1"></a>

### receive_response/1 ###

<pre><code>
receive_response(RequestId::<a href="#type-request_id">request_id()</a>) -&gt; {ok, term()} | <a href="#type-error">error()</a>
</code></pre>
<br />

<a name="request-3"></a>

### request/3 ###

<pre><code>
request(Method::<a href="#type-method">method()</a>, Buoy_url::<a href="#type-buoy_url">buoy_url()</a>, BuoyOpts::<a href="#type-buoy_opts">buoy_opts()</a>) -&gt; {ok, <a href="#type-buoy_resp">buoy_resp()</a>} | <a href="#type-error">error()</a>
</code></pre>
<br />

