%%%-------------------------------------------------------------------
%% @doc radar public API
%% @end
%%%-------------------------------------------------------------------

-module(radar_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    radar:start_link().

stop(_State) ->
    wx:destroy(),
    ok.

%% internal functions
