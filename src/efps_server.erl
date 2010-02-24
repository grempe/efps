-module(efps_server).
-author('Glenn Rempe <glenn@rempe.us>').
-behaviour(gen_listener_tcp).

%% Implements a Flash Socket Policy File server using the gen_listener_tcp behaviour.

-define(TCP_PORT, 843).
-define(TCP_OPTS, [binary, inet,
                   {active,    once},
                   {backlog,   100},
                   {nodelay,   true},
                   {packet,    raw},
                   {reuseaddr, true}]).

%% API
-export([start_link/0]).

%% gen_listener_tcp callbacks
-export([init/1,
         handle_accept/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% @doc Start the server.
start_link() ->
    gen_listener_tcp:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc The flash policy file client.
fps_client(Socket, State) ->
    %error_logger:info_msg("client()~n"),
    %error_logger:info_msg("State : domain ~p : ports ~p ~n", [proplists:get_value(domain,State,"*"), proplists:get_value(ports,State,"*")]),
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"<policy-file-request/>", _R/binary>>} ->
            %error_logger:info_msg("Policy File Requested."),
            
            Reply   = "<?xml version=\"1.0\"?>"
                    ++ "<!DOCTYPE cross-domain-policy SYSTEM \"/xml/dtds/cross-domain-policy.dtd\">"
                    ++ "<cross-domain-policy>"
                    ++ "<allow-access-from domain=\"" ++ [proplists:get_value(domain,State,"*")] ++ "\" to-ports=\"" ++ [proplists:get_value(ports,State,"*")] ++ "\" />"
                    ++ "</cross-domain-policy>\0",
            gen_tcp:send(Socket, Reply ++ "\r\n"),
            gen_tcp:close(Socket);
        {tcp, Socket, Data} ->
            error_logger:info_msg("Unexpected Request : ~p", [Data]),
            gen_tcp:send(Socket, "<error/>\r\n"),
            gen_tcp:close(Socket);
        {tcp_closed, Socket} ->
            error_logger:info_msg("Client Disconnected.")
    end.

init([]) ->
    {ok, Conf} = file:consult("application.cfg"),
    {ok, {?TCP_PORT, ?TCP_OPTS}, Conf}.

handle_accept(Sock, State) ->
    Pid = spawn(fun() -> fps_client(Sock, State) end),
    gen_tcp:controlling_process(Sock, Pid),
    {noreply, State}.

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

