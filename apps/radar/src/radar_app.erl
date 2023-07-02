%%%-------------------------------------------------------------------
%% @doc radar public API
%% @end
%%%-------------------------------------------------------------------

-module(radar_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {_,_,_,PID} = radar:start_link(),
    {ok, PID}.

stop(_State) ->
    wx:destroy(),
    ok.

%% internal functions
