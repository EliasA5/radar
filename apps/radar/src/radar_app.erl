%%%-------------------------------------------------------------------
%% @doc radar public API
%% @end
%%%-------------------------------------------------------------------

-module(radar_app).

-behaviour(application).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    radar_app:start_link().

stop(_State) ->
    wx:destroy(),
    ok.

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 1,
                 auto_shutdown => any_significant
                },
    ChildSpecs = [#{id => radar,
                    start => {radar, start_link, []},
                    restart => transient,
                    significant => true
                   }],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

