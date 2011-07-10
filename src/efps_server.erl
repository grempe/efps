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
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"<policy-file-request/>", _R/binary>>} ->
            %error_logger:info_msg("Policy File Requested."),

          Reply   = ["<?xml version=\"1.0\"?>\n",
                     "<!DOCTYPE cross-domain-policy SYSTEM \"/xml/dtds/cross-domain-policy.dtd\">\n",
                     "<cross-domain-policy>\n",
                     lists:map(fun write_element/1, State),
                     "</cross-domain-policy>\0"],

            %error_logger:info_msg("Sending Reply : ~p~n", [list_to_binary(Reply)]),

            gen_tcp:send(Socket, iolist_to_binary(Reply ++ "\r\n")),
            gen_tcp:close(Socket);
        {tcp, Socket, Data} ->
            error_logger:info_msg("Unexpected Request : ~p", [Data]),
            gen_tcp:send(Socket, "<error/>\r\n"),
            gen_tcp:close(Socket);
        {tcp_closed, Socket} ->
            error_logger:info_msg("Client Disconnected.")
    end.

write_element({site_control, Attributes}) ->
  io_lib:format(
    "<site-control permitted-cross-domain-policies=\"~s\" />~n",
    [proplists:get_value(permitted_cross_domain_policies, Attributes, "none")]);
write_element({allow_access_from, Attributes}) ->
  io_lib:format(
    "<allow-access-from domain=\"~s\" to-ports=\"~s\" secure=\"~p\"/>~n",
    [proplists:get_value(domain, Attributes, "*"),
     proplists:get_value(to_ports, Attributes, "*"),
     proplists:get_bool(secure, Attributes)]);
write_element({allow_access_from_identity, {signatory, {certificate, Attributes}}}) ->
  io_lib:format(
    "<allow-access-from-identity>~n"
      "\t<signatory>~n"
        "\t\t<certificate fingerprint-algorithm=\"~s\" fingerprint=\"~s\" />~n"
          "\t</signatory>~n"
            "</allow-access-from-identity>~n",
    [proplists:get_value(fingerprint_algorithm, Attributes, "sha-1"),
     proplists:get_value(fingerprint, Attributes, "")]);
write_element({allow_http_request_headers_from, Attributes}) ->
  io_lib:format(
    "<allow-http-request-headers-from domain=\"~s\" headers=\"~s\" secure=\"~p\"/>~n",
    [proplists:get_value(domain, Attributes, "*"),
     proplists:get_value(headers, Attributes, "*"),
     proplists:get_bool(secure, Attributes)]);
write_element({included_applications, _}) -> [];
write_element(Element = {Name, _}) ->
  error_logger:warning_msg("Unsupported element in efps configuration: ~p", [Element]),
  io_lib:format("<!-- Unsupported element: ~p-->", [Name]);
write_element(Element) ->
  error_logger:warning_msg("Unsupported element in efps configuration: ~p", [Element]),
  [].

init([]) ->
    Conf = application:get_all_env(),
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

