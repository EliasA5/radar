%%%-------------------------------------------------------------------
%%% @author Elias Assaf <elias>
%%% @copyright (C) 2023, Elias Assaf
%%% @doc
%%%
%%% @end
%%% Created: 08 August 2023
%%%-------------------------------------------------------------------
-module(communication_img).

-behaviour(gen_statem).

-include("inotify/include/inotify.hrl").

-include("include/defs.hrl").

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([handle_event/4]).

%% inotify events
-export([inotify_event/3]).

-define(SERVER, ?MODULE).

-define(LOWPROB, 0.2).

-define(MIDPROB, 0.5).

-define(HIGHPROB, 0.8).

-record(data, {file, operator_port, inotify_ref,
               intensity, angle = 0, curr_file = <<>>, file_replace = 0,
               files = {<<>>, <<>>, <<>>}}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: list()) ->
  {ok, Pid :: pid()} |
  ignore |
  {error, Error :: term()}.
start_link(Args) ->
  gen_statem:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Define the callback_mode() for this callback module.
%% @end
%%--------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() -> handle_event_function.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
  gen_statem:init_result(term()).

init([]) ->
  {error, no_port_given};
init(Args) ->
  process_flag(trap_exit, true),
  case proplists:get_value(port_file, Args) of
    undefined ->
      {error, no_port_given};
    PortFile ->
      OperatorPort = proplists:get_value(operator, Args, operator),
      Ref = inotify:watch(PortFile, [delete_self]),
      inotify:add_handler(Ref, ?MODULE, self()),
      {ok, idle, #data{file = PortFile, operator_port = OperatorPort, inotify_ref = Ref}}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for every event a gen_statem receives.
%% possible states: [idle, scan_us, scan_ldr, scan_both, telemeter, do_file]
%% @end
%%--------------------------------------------------------------------
-spec handle_event('enter',
                   OldState :: term(),
                   State :: term(),
                   Data :: term()) ->
  gen_statem:state_enter_result(term());
                  (gen_statem:event_type(),
                   Msg :: term(),
                   State :: term(),
                   Data :: term()) ->
  gen_statem:event_handler_result(term()).

handle_event(cast, {inotify, _Arg, _EventTag, _Masks, _Filename}, _State, Data) ->
  inotify:unwatch(Data#data.inotify_ref),
  {stop, normal};

handle_event(cast, {send, ?IDLE_CMD, []}, _State, Data) ->
  {next_state, idle, Data#data{angle = 0}};

handle_event(cast, {send, ?SONIC_D_CMD, []}, _State, Data) ->
  {next_state, scan_us, Data#data{angle = 0}};

handle_event(cast, {send, ?LDR_D_CMD, []}, _State, Data) ->
  {next_state, scan_ldr, Data#data{angle = 0}};

handle_event(cast, {send, ?DUAL_D_CMD, []}, _State, Data) ->
  {next_state, scan_both, Data#data{angle = 0}};

handle_event(cast, {send, telemeter, Angle}, _State, Data) ->
  {next_state, telemeter, Data#data{angle = Angle}};

handle_event(cast, {send, ?FILE_0_CMD, []}, _State, #data{files = {File, _, _}} = Data) ->
  {next_state, do_file, Data#data{angle = 0, curr_file = File}};

handle_event(cast, {send, ?FILE_1_CMD, []}, _State, #data{files = {_, File, _}} = Data) ->
  {next_state, do_file, Data#data{angle = 0, curr_file = File}};

handle_event(cast, {send, ?FILE_2_CMD, []}, _State, #data{files = {_, _, File}} = Data) ->
  {next_state, do_file, Data#data{angle = 0, curr_file = File}};

handle_event(cast, {send, file, ParsedFile}, _State,
              #data{file_replace = FileReplace, files = Files} = Data) ->
  {next_state, idle, Data#data{files = setelement(FileReplace, Files, ParsedFile),
                               file_replace = (FileReplace + 1) rem 3}};

handle_event(info, advance, idle, _Data) ->
  keep_state_and_data;

handle_event(info, advance, State, Data) ->
  Sample = generate_random_sample(State, Data),
  %% update data angle
  {next_state, scan_us, Data}.

inotify_event(Arg, EventTag, ?inotify_msg(Masks, _Cookie, Filename)) ->
  gen_statem:cast(Arg, {inotify, Arg, EventTag, Masks, Filename}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: term(), Data :: term()) ->
  any().
terminate(_Reason, _State, _Data) ->
  void.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(
        OldVsn :: term() | {down,term()},
        State :: term(), Data :: term(), Extra :: term()) ->
  {ok, NewState :: term(), NewData :: term()} |
  (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
  {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% ultrasonic sample format
% Angle [0, 180], Distrance [2, 450]
% {ultrasonic, self(), {Angle, Distance}}

% ldr sample format
% Angle [0, 180], Distrance [2, 50]
% {ldr, self(), {Angle, Distance}}

generate_random_sample(State, #data{intensity = Intensity, angle = Angle} = Data) ->
  RadiusMean = 2*rand:uniform(25),
  RadiusVariance = rand:normal(0, 1),
  Radius = RadiusMean + RadiusVariance,
  SampleProb = case Intensity of
    low -> ?HIGHPROB;
    mid -> ?MIDPROB;
    high -> ?HIGHPROB
  end,
  case (rand:uniform()) of
    Prob when Prob > SampleProb ->
      ok;
    _ ->
      {Radius, Angle}
   end.

