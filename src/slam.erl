-module(slam).
-export([start/0, tests/2, run_test/3]).

start() ->
    tests(843, 0).

% start up 10 concurrent clients that will each run tests
tests(Port, 10) -> ok;
tests(Port, ClientNum) ->
    TestCount = 0,
    spawn(fun() -> run_test(Port, ClientNum, TestCount) end),
    io:format("Spawned Client # ~B ~n", [ClientNum]),
    tests(Port, ClientNum + 1).

run_test(Port, ClientNum, 100) -> ok;
run_test(Port, ClientNum, Count) ->
    StartTime={SMegaseconds,SSeconds,SMicroseconds} = erlang:now(),
    case gen_tcp:connect("127.0.0.1", Port, [list]) of
    {ok, Socket} ->
        gen_tcp:send(Socket, "<policy-file-request/>"),
        Reply = wait_reply(Socket),
        EndTime={EMegaseconds,ESeconds,EMicroseconds} = erlang:now(),
        MegaSeconds = EMegaseconds - SMegaseconds,
        Seconds = ESeconds - SSeconds,
        Microseconds = EMicroseconds - SMicroseconds,
        io:format("~B:~B -- ~B:~B:~B~n", [ClientNum, Count, MegaSeconds, Seconds, Microseconds]),
        gen_tcp:close(Socket),
        run_test(Port, ClientNum, Count + 1);
    _ ->
        error
    end.

wait_reply(_X) ->
    receive
    Reply ->
        {value, Reply}
    after 100000 ->
        timeout
    end.

