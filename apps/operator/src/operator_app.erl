%%%-------------------------------------------------------------------
%% @doc operator public API and top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(operator_app).

-behaviour(application).

-export([start/2, stop/1]).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start(_StartType, _StartArgs) ->
  operator_app:start_link().

stop(_State) ->
  net_kernel:stop(),
  ok.

%%%-------------------------------------------------------------------
%% @end app
%%%-------------------------------------------------------------------

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
    SupFlags = #{strategy => rest_for_one,
                 intensity => 1,
                 period => 1
                },
    % make operator node important and shutdown everything if it does
    ChildSpecs = [
                  #{id => inotify_evt,
                   start => {inotify_evt, start_link, []},
                   modules => [inotify_evt]
                   },
                  #{id => inotify_server,
                   start => {inotify_server, start_link, []},
                   modules => [inotify_server]
                   },
                  #{id => operator,
                   start => {operator, start_link, [supervisor]}
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.


%% internal functions

