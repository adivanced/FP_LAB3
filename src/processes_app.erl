%%%-------------------------------------------------------------------
%% @doc processes public API
%% @end
%%%-------------------------------------------------------------------

-module(processes_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    processes_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
