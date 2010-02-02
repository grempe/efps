-module(efps_sup).
-author('Glenn Rempe <glenn@rempe.us>').
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    EfpsChild = {efps_server, {efps_server, start_link, []}, permanent, 10000, worker, [efps_server]},
    {ok,{{one_for_one, 5, 10},[EfpsChild]}}.

