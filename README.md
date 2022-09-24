# buoy

High-Performance Erlang HTTP 1.1 Client

[![Build Status](https://travis-ci.org/lpgauth/buoy.svg?branch=dev)](https://travis-ci.org/lpgauth/buoy.svg?branch=dev)
[![Coverage Status](https://coveralls.io/repos/github/lpgauth/buoy/badge.svg?branch=dev)](https://coveralls.io/github/lpgauth/buoy?branch=dev)

## Disclaimer:
This HTTP client has been designed for HTTP 1.1 with keep-alive. For performance reasons, it only implements a subset of RFC2616.

### Unsupported Features:

- Doesn't accept an arbitrary number of new lines in headers
- Doesn't accept random capitalization of content-length header
- Doesn't protect against malicious servers

## API
<a href="http://github.com/lpgauth/buoy/blob/master/doc/buoy.md#index" class="module">Function Index</a>

## Examples

```erlang
1> buoy_app:start().
{ok,[shackle,buoy]}

2> Url = buoy_utils:parse_url(<<"http://example.com">>).
{buoy_url,<<"example.com">>,<<"example.com">>,<<"/">>,80,
          http}

3> ok = buoy_pool:start(Url, [{pool_size, 1}]).
ok

4> {ok, Resp} = buoy:get(Url, #{timeout => 500}).
{ok,{buoy_resp,done,
               <<"<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta charset=\"utf-8\" />\n  "...>>,
               1270,
               [<<"Cache-Control: max-age=604800">>,
                <<"Content-Type: text/html">>,
                <<"Date: Mon, 20 Mar 2017 14:48:25 GMT">>,
                <<"Etag: \"359670651+gzip+ident\"">>,
                <<"Expires: Mon, 27 Mar 2017 14:48:25 GMT">>,
                <<"Last-Modified: Fri, 09 Aug 2013 23:54:35 GMT">>,
                <<"Server: ECS (cpm/F9D5)">>,<<"Vary: Accept-Encoding">>,
                <<"X-Cache: HIT">>,<<"Content-Length: 1270">>],
               <<"OK">>,200}}

5> {ok, Headers} = buoy_protocol:headers(Resp).
{ok,[{<<"Cache-Control">>,<<"max-age=604800">>},
     {<<"Content-Type">>,<<"text/html">>},
     {<<"Date">>,<<"Mon, 20 Mar 2017 14:48:25 GMT">>},
     {<<"Etag">>,<<"\"359670651+gzip+ident\"">>},
     {<<"Expires">>,<<"Mon, 27 Mar 2017 14:48:25 GMT">>},
     {<<"Last-Modified">>,<<"Fri, 09 Aug 2013 23:54:35 GMT">>},
     {<<"Server">>,<<"ECS (cpm/F9D5)">>},
     {<<"Vary">>,<<"Accept-Encoding">>},
     {<<"X-Cache">>,<<"HIT">>},
     {<<"Content-Length">>,<<"1270">>}]}
```

### Pool Options

<table width="100%">
  <theader>
    <th>Name</th>
    <th>Type</th>
    <th>Default</th>
    <th>Description</th>
  </theader>
  <tr>
    <td>backlog_size</td>
    <td>pos_integer()</td>
    <td>1024</td>
    <td>maximum number of concurrent requests per connection</td>
  </tr>
  <tr>
    <td>pool_size</td>
    <td>pos_integer()</td>
    <td>16</td>
    <td>number of connections</td>
  </tr>
  <tr>
    <td>pool_strategy</td>
    <td>random | round_robin</td>
    <td>random</td>
    <td>connection selection strategy</td>
  </tr>
  <tr>
    <td>reconnect</td>
    <td>boolean()</td>
    <td>true</td>
    <td>reconnect closed connections</td>
  </tr>
  <tr>
    <td>reconnect_time_max</td>
    <td>pos_integer() | infinity</td>
    <td>120000</td>
    <td>reconnect maximum time</td>
  </tr>
  <tr>
    <td>reconnect_time_min</td>
    <td>pos_integer()</td>
    <td>500</td>
    <td>reconnect minimum time</td>
  </tr>
  <tr>
    <td>socket_options</td>
    <td>[gen_tcp:connect_option() | ssl:tls_client_option()]</td>
    <td>[binary,
    {packet, line},
    {packet, raw},
    {send_timeout, 50},
    {send_timeout_close, true}]</td>
    <td>options passed to the socket when connecting</td>
  </tr>
</table>

## Tests

```makefile
make dialyzer
make elvis
make eunit
make xref
```

## License
```license
The MIT License (MIT)

Copyright (c) 2016-2017 Louis-Philippe Gauthier

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
