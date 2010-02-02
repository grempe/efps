-module(efps_app).
-author('Glenn Rempe <glenn@rempe.us>').
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    efps_sup:start_link().

stop(_State) ->
    ok.


