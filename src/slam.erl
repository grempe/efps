-module(slam).
-export([start/0, start/1, tests/2]).

start() ->
    tests(843, 100).
start(Num) ->
    tests(843, Num).

tests(Port, 0) -> ok;
tests(Port, Num) ->
    TestCount = 0,
    spawn(fun() -> client(Port) end),
    %error_logger:info_msg("Spawned Client # ~B ~n", [Num]),
    tests(Port, Num - 1).

client(Port) ->
    case gen_tcp:connect("127.0.0.1", Port, []) of
    {ok, Socket} ->
        case gen_tcp:send(Socket, "<policy-file-request/>") of
        ok ->
            %error_logger:info_msg("Request sent~n"),
            wait_reply(Socket);
        {error, closed} ->
            error_logger:info_msg("Request failed with {error, closed}~n");
        _ ->
            error_logger:info_msg("Request failed with unknown error~n")
        end,
        ok = gen_tcp:close(Socket);
    _ ->
        error
    end.

wait_reply(_X) ->
    receive
    Reply ->
        case Reply of
        {tcp,_Port,_Msg} ->
            %error_logger:info_msg("Reply received~n");
            ok;
        {tcp_closed,_Port} ->
            error_logger:error_msg("tcp_closed error~n");
        _ ->
            error_logger:error_msg("Reply received with unknown error~n"),
            error
        end,
        {value, Reply}
    after 1000 ->
        timeout
    end.

