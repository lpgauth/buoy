

# Module buoy_pool #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-backlog_size">backlog_size()</a> ###


<pre><code>
backlog_size() = pos_integer() | infinity
</code></pre>




### <a name="type-buoy_resp">buoy_resp()</a> ###


<pre><code>
buoy_resp() = #buoy_resp{state = body | done, status_code = undefined | 100..505, reason = undefined | binary(), headers = undefined | [binary()], content_length = undefined | non_neg_integer(), body = undefined | binary()}
</code></pre>




### <a name="type-buoy_url">buoy_url()</a> ###


<pre><code>
buoy_url() = #buoy_url{scheme = <a href="#type-scheme">scheme()</a>, host = <a href="#type-host">host()</a>, hostname = <a href="#type-hostname">hostname()</a>, port = <a href="inet.md#type-port_number">inet:port_number()</a>, path = <a href="#type-path">path()</a>}
</code></pre>




### <a name="type-client_option">client_option()</a> ###


<pre><code>
client_option() = {ip, <a href="inet.md#type-ip_address">inet:ip_address()</a> | <a href="inet.md#type-hostname">inet:hostname()</a>} | {port, <a href="inet.md#type-port_number">inet:port_number()</a>} | {protocol, <a href="#type-protocol">protocol()</a>} | {reconnect, boolean()} | {reconnect_time_max, <a href="#type-time">time()</a>} | {reconnect_time_min, <a href="#type-time">time()</a>} | {socket_options, [<a href="gen_tcp.md#type-connect_option">gen_tcp:connect_option()</a> | <a href="gen_udp.md#type-option">gen_udp:option()</a>]}
</code></pre>




### <a name="type-client_options">client_options()</a> ###


<pre><code>
client_options() = [<a href="#type-client_option">client_option()</a>]
</code></pre>




### <a name="type-host">host()</a> ###


<pre><code>
host() = binary()
</code></pre>




### <a name="type-hostname">hostname()</a> ###


<pre><code>
hostname() = binary()
</code></pre>




### <a name="type-option">option()</a> ###


<pre><code>
option() = backlog_size | pool_size | pool_strategy | reconnect | reconnect_time_max | reconnect_time_min
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = [<a href="#type-option">option()</a>]
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
protocol() = shackle_tcp | shackle_udp
</code></pre>




### <a name="type-scheme">scheme()</a> ###


<pre><code>
scheme() = http | https
</code></pre>




### <a name="type-time">time()</a> ###


<pre><code>
time() = pos_integer()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#start-3">start/3</a></td><td></td></tr><tr><td valign="top"><a href="#stop-2">stop/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-0"></a>

### init/0 ###

<pre><code>
init() -&gt; ok
</code></pre>
<br />

<a name="start-2"></a>

### start/2 ###

<pre><code>
start(Host::<a href="#type-hostname">hostname()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -&gt; ok | {error, pool_already_started | shackle_not_started}
</code></pre>
<br />

<a name="start-3"></a>

### start/3 ###

<pre><code>
start(Host::<a href="#type-hostname">hostname()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>, Options::<a href="#type-options">options()</a>) -&gt; ok | {error, pool_already_started | shackle_not_started}
</code></pre>
<br />

<a name="stop-2"></a>

### stop/2 ###

<pre><code>
stop(Host::<a href="#type-hostname">hostname()</a>, Port::<a href="inet.md#type-port_number">inet:port_number()</a>) -&gt; ok | {error, shackle_not_started | pool_not_started}
</code></pre>
<br />

