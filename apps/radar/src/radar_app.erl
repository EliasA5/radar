%%%-------------------------------------------------------------------
%% @doc radar public API
%% @end
%%%-------------------------------------------------------------------

-module(radar_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    radar_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
