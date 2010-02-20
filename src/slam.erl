-module(slam).
-export([start/0, tests/2]).

start() ->
    tests(843, 100).

tests(Port, 0) -> ok;
tests(Port, Num) ->
    TestCount = 0,
    spawn(fun() -> client(Port) end),
    io:format("Spawned Client # ~B ~n", [Num]),
    tests(Port, Num - 1).

client(Port) ->
    case gen_tcp:connect("127.0.0.1", Port, [list]) of
    {ok, Socket} ->
        gen_tcp:send(Socket, "<policy-file-request/>"),
        Reply = wait_reply(Socket),
        io:format("Client completed with Reply: ~P ~n", Reply),
        gen_tcp:close(Socket);
    _ ->
        error
    end.

wait_reply(_X) ->
    receive
    Reply ->
        {value, Reply}
    after 1000 ->
        timeout
    end.

