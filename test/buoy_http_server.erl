-module(buoy_http_server).

-export([
    start/0,
    stop/0
]).

-define(PORT, 8080).

%% public
start() ->
    Self = self(),
    Pid = spawn(fun () -> init(Self) end),
    receive
        {Pid, started} ->
            {ok, Pid}
    after 5000 ->
        {error, timeout}
    end.

stop() ->
    case whereis(?MODULE) of
        undefined ->
            ok;
        Pid ->
            exit(Pid, kill),
            ok
    end.

%% private
init(Parent) ->
    register(?MODULE, self()),
    {ok, LSocket} = gen_tcp:listen(?PORT, [
        binary,
        {active, false},
        {backlog, 128},
        {packet, http_bin},
        {reuseaddr, true}
    ]),
    Parent ! {self(), started},
    accept(LSocket).

accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    Pid = spawn_link(fun () ->
        receive go -> connection(Socket) end
    end),
    ok = gen_tcp:controlling_process(Socket, Pid),
    Pid ! go,
    accept(LSocket).

connection(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_request, Method, {abs_path, Path}, _Version}} ->
            ContentLength = headers(Socket, 0),
            Body = body(Socket, ContentLength),
            respond(Socket, Method, Path, Body),
            connection(Socket);
        {ok, _} ->
            gen_tcp:close(Socket);
        {error, _} ->
            gen_tcp:close(Socket)
    end.

headers(Socket, ContentLength) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_header, _, 'Content-Length', _, Value}} ->
            headers(Socket, binary_to_integer(Value));
        {ok, {http_header, _, _, _, _}} ->
            headers(Socket, ContentLength);
        {ok, http_eoh} ->
            ContentLength
    end.

body(_Socket, 0) ->
    <<>>;
body(Socket, ContentLength) ->
    ok = inet:setopts(Socket, [{packet, raw}]),
    {ok, Body} = gen_tcp:recv(Socket, ContentLength),
    ok = inet:setopts(Socket, [{packet, http_bin}]),
    Body.

respond(Socket, Method, <<"/1">>, _Body) ->
    reply(Socket, Method, <<"Hello world!">>);
respond(Socket, Method, <<"/2">>, _Body) ->
    reply(Socket, Method, binary:copy(<<"Hello world!">>, 1000));
respond(Socket, Method, <<"/3">>, Body) ->
    reply(Socket, Method, Body);
respond(Socket, Method, <<"/4">>, _Body) ->
    chunked_reply(Socket, Method, [<<"Hello">>, <<" world!">>]);
respond(Socket, Method, <<"/5">>, _Body) ->
    reply(Socket, Method, method(Method)).

method(Method) when is_atom(Method) ->
    atom_to_binary(Method, utf8);
method(Method) when is_binary(Method) ->
    Method.

reply(Socket, Method, Body) ->
    Headers = [
        <<"HTTP/1.1 200 OK\r\n">>,
        <<"Connection: Keep-Alive\r\n">>,
        <<"Content-Type: text/plain\r\n">>,
        <<"Content-Length: ">>, integer_to_binary(iolist_size(Body)),
        <<"\r\n\r\n">>
    ],
    case Method of
        'HEAD' ->
            ok = gen_tcp:send(Socket, Headers);
        _ ->
            ok = gen_tcp:send(Socket, [Headers, Body])
    end.

chunked_reply(Socket, Method, Chunks) ->
    Headers = [
        <<"HTTP/1.1 200 OK\r\n">>,
        <<"Connection: Keep-Alive\r\n">>,
        <<"Content-Type: text/plain\r\n">>,
        <<"Transfer-Encoding: chunked\r\n\r\n">>
    ],
    case Method of
        'HEAD' ->
            ok = gen_tcp:send(Socket, Headers);
        _ ->
            Encoded = [[integer_to_binary(byte_size(Chunk), 16), <<"\r\n">>,
                Chunk, <<"\r\n">>] || Chunk <- Chunks],
            ok = gen_tcp:send(Socket, [Headers, Encoded, <<"0\r\n\r\n">>])
    end.
