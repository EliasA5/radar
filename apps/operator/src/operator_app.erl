%%%-------------------------------------------------------------------
%% @doc operator public API
%% @end
%%%-------------------------------------------------------------------

-module(operator_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    operator_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
