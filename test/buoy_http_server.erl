-module(buoy_http_server).

-export([
    connection_count/0,
    start/0,
    stop/0
]).

-define(PORT, 8080).
-define(PORT_SSL, 8443).
-define(LISTEN_OPTIONS, [
    binary,
    {active, false},
    {backlog, 128},
    {packet, http_bin},
    {reuseaddr, true}
]).

%% public
connection_count() ->
    counters:get(persistent_term:get({?MODULE, connections}), 1).

start() ->
    Self = self(),
    Pid = spawn(fun () -> init(Self) end),
    receive
        {Pid, started} ->
            {ok, Pid};
        {Pid, {error, Reason}} ->
            {error, Reason}
    after 5000 ->
        {error, timeout}
    end.

stop() ->
    case whereis(?MODULE) of
        undefined ->
            ok;
        Pid ->
            Ref = monitor(process, Pid),
            exit(Pid, kill),
            receive
                {'DOWN', Ref, process, Pid, _} ->
                    ok
            end
    end.

%% private
init(Parent) ->
    try
        register(?MODULE, self()),
        persistent_term:put({?MODULE, connections}, counters:new(1, [])),
        {ok, _} = application:ensure_all_started(ssl),
        {ok, LSocket} = listen(gen_tcp, ?PORT, ?LISTEN_OPTIONS),
        %% the default pkix_test_data key (secp112r2, sha1) is not
        %% negotiable by a modern TLS client
        KeyOpts = [{key, {namedCurve, secp256r1}}, {digest, sha256}],
        SslOptions = public_key:pkix_test_data(#{root => KeyOpts,
            peer => KeyOpts}),
        {ok, LSocketSsl} = listen(ssl, ?PORT_SSL,
            ?LISTEN_OPTIONS ++ SslOptions),
        spawn_link(fun () -> accept_ssl(LSocketSsl) end),
        Parent ! {self(), started},
        accept(LSocket)
    catch
        Class:Error:Stacktrace ->
            Parent ! {self(), {error, {Class, Error, Stacktrace}}}
    end.

%% the previous fixture's ports can linger briefly after its death:
%% ERTS releases them asynchronously once the DOWN signal fires
listen(Transport, Port, Options) ->
    listen(Transport, Port, Options, 50).

listen(Transport, Port, Options, Retries) ->
    case Transport:listen(Port, Options) of
        {ok, LSocket} ->
            {ok, LSocket};
        {error, eaddrinuse} when Retries > 0 ->
            timer:sleep(10),
            listen(Transport, Port, Options, Retries - 1);
        {error, _} = E ->
            E
    end.

accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    counters:add(persistent_term:get({?MODULE, connections}), 1, 1),
    Pid = spawn_link(fun () ->
        receive go -> connection(gen_tcp, Socket) end
    end),
    ok = gen_tcp:controlling_process(Socket, Pid),
    Pid ! go,
    accept(LSocket).

accept_ssl(LSocket) ->
    {ok, TSocket} = ssl:transport_accept(LSocket),
    counters:add(persistent_term:get({?MODULE, connections}), 1, 1),
    Pid = spawn_link(fun () ->
        receive
            go ->
                case ssl:handshake(TSocket) of
                    {ok, Socket} ->
                        connection(ssl, Socket);
                    {error, _} ->
                        ok
                end
        end
    end),
    ok = ssl:controlling_process(TSocket, Pid),
    Pid ! go,
    accept_ssl(LSocket).

connection(Transport, Socket) ->
    case recv(Transport, Socket, 0) of
        {ok, {http_request, Method, {abs_path, Path}, _Version}} ->
            ContentLength = headers(Transport, Socket, 0),
            Body = body(Transport, Socket, ContentLength),
            respond(Transport, Socket, Method, Path, Body),
            connection(Transport, Socket);
        {ok, _} ->
            close(Transport, Socket);
        {error, _} ->
            close(Transport, Socket)
    end.

headers(Transport, Socket, ContentLength) ->
    case recv(Transport, Socket, 0) of
        {ok, {http_header, _, 'Content-Length', _, Value}} ->
            headers(Transport, Socket, binary_to_integer(Value));
        {ok, {http_header, _, _, _, _}} ->
            headers(Transport, Socket, ContentLength);
        {ok, http_eoh} ->
            ContentLength
    end.

body(_Transport, _Socket, 0) ->
    <<>>;
body(Transport, Socket, ContentLength) ->
    ok = setopts(Transport, Socket, [{packet, raw}]),
    {ok, Body} = recv(Transport, Socket, ContentLength),
    ok = setopts(Transport, Socket, [{packet, http_bin}]),
    Body.

respond(Transport, Socket, Method, <<"/1">>, _Body) ->
    reply(Transport, Socket, Method, <<"Hello world!">>);
respond(Transport, Socket, Method, <<"/2">>, _Body) ->
    reply(Transport, Socket, Method, binary:copy(<<"Hello world!">>, 1000));
respond(Transport, Socket, Method, <<"/3">>, Body) ->
    reply(Transport, Socket, Method, Body);
respond(Transport, Socket, Method, <<"/4">>, _Body) ->
    chunked_reply(Transport, Socket, Method, [<<"Hello">>, <<" world!">>]);
respond(Transport, Socket, Method, <<"/5">>, _Body) ->
    reply(Transport, Socket, Method, method(Method)).

method(Method) when is_atom(Method) ->
    atom_to_binary(Method, utf8);
method(Method) when is_binary(Method) ->
    Method.

reply(Transport, Socket, Method, Body) ->
    Headers = [
        <<"HTTP/1.1 200 OK\r\n">>,
        <<"Connection: Keep-Alive\r\n">>,
        <<"Content-Type: text/plain\r\n">>,
        <<"Content-Length: ">>, integer_to_binary(iolist_size(Body)),
        <<"\r\n\r\n">>
    ],
    case Method of
        'HEAD' ->
            ok = send(Transport, Socket, Headers);
        _ ->
            ok = send(Transport, Socket, [Headers, Body])
    end.

chunked_reply(Transport, Socket, Method, Chunks) ->
    Headers = [
        <<"HTTP/1.1 200 OK\r\n">>,
        <<"Connection: Keep-Alive\r\n">>,
        <<"Content-Type: text/plain\r\n">>,
        <<"Transfer-Encoding: chunked\r\n\r\n">>
    ],
    case Method of
        'HEAD' ->
            ok = send(Transport, Socket, Headers);
        _ ->
            Encoded = [[integer_to_binary(byte_size(Chunk), 16), <<"\r\n">>,
                Chunk, <<"\r\n">>] || Chunk <- Chunks],
            ok = send(Transport, Socket, [Headers, Encoded, <<"0\r\n\r\n">>])
    end.

close(gen_tcp, Socket) ->
    gen_tcp:close(Socket);
close(ssl, Socket) ->
    ssl:close(Socket).

recv(gen_tcp, Socket, Length) ->
    gen_tcp:recv(Socket, Length);
recv(ssl, Socket, Length) ->
    ssl:recv(Socket, Length).

send(gen_tcp, Socket, Data) ->
    gen_tcp:send(Socket, Data);
send(ssl, Socket, Data) ->
    ssl:send(Socket, Data).

setopts(gen_tcp, Socket, Opts) ->
    inet:setopts(Socket, Opts);
setopts(ssl, Socket, Opts) ->
    ssl:setopts(Socket, Opts).
