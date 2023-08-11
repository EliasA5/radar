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
               curr_file = <<>>, file_replace = 0, files = {<<>>, <<>>, <<>>},
               intensity = ?LOWPROB, angle = 0, ldr_r = 50, ldr_v = 10, us_r = 450, us_v = 10
              }).

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
      timer:send_interval(100, advance),
      timer:send_interval(1000, update_randomness),
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

handle_event(internal, {continue, Continue}, _State, Data) when is_list(Continue) ->
  NewData = lists:foldl(fun do_cont/2, Data, Continue),
  {keep_state, NewData};

handle_event(internal, {continue, Continue}, _State, Data) ->
  {keep_state, do_cont(Continue, Data)};

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

handle_event(info, update_randomness, _State, Data) ->
  UsR =  5 + rand:uniform(444),
  LdrR = 15 + rand:uniform(75),
  UsV = 0.1 * UsR,
  LdrV = 0.1 * LdrR,
  Intensity = case rand:uniform(3) of
    1 -> ?LOWPROB;
    2 -> ?MIDPROB;
    3 -> ?HIGHPROB
    end,
  {keep_state, Data#data{intensity = Intensity, us_r = UsR, ldr_r = LdrR, us_v = UsV, ldr_v = LdrV}};

% [idle, scan_us, scan_ldr, scan_both, telemeter, do_file]

handle_event(info, advance, scan_us, #data{angle = Angle} = Data) ->
  case generate_random_sample(us, Data) of
    no_sample -> ok;
    {_Angle, _Dist} = Sample ->
      gen_server:cast(Data#data.operator_port, {ultrasonic, self(), Sample})
  end,
  %% update data angle
  {keep_state, Data#data{angle = (Angle + 3) rem 180}};

handle_event(info, advance, telemeter, Data) ->
  case generate_random_sample(us, Data) of
    no_sample -> ok;
    {_Angle, _Dist} = Sample ->
      gen_server:cast(Data#data.operator_port, {ultrasonic, self(), Sample})
  end,
  %% update data angle
  {keep_state, Data};

handle_event(info, advance, scan_ldr, #data{angle = Angle} = Data) ->
  case generate_random_sample(ldr, Data) of
    no_sample -> ok;
    {_Angle, _Dist} = Sample ->
      gen_server:cast(Data#data.operator_port, {ldr, self(), Sample})
  end,
  %% update data angle
  {keep_state, Data#data{angle = (Angle + 3) rem 180}};

handle_event(info, advance, scan_both, #data{angle = Angle} = Data) ->
  case generate_random_sample(us, Data) of
    no_sample -> ok;
    Sample_US ->
      gen_server:cast(Data#data.operator_port, {ultrasonic, self(), Sample_US})
  end,
  case generate_random_sample(ldr, Data) of
    no_sample -> ok;
    Sample_LDR ->
      gen_server:cast(Data#data.operator_port, {ldr, self(), Sample_LDR})
  end,
  %% update data angle
  {keep_state, Data#data{angle = (Angle + 3) rem 180}};

handle_event(info, advance, do_file, Data) ->
  %% update data angle
  {keep_state, Data}.

inotify_event(Arg, EventTag, ?inotify_msg(Masks, _Cookie, Filename)) ->
  gen_statem:cast(Arg, {inotify, Arg, EventTag, Masks, Filename}).

do_cont(_Continue, Data) ->
  Data.
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

generate_random_sample(us, #data{intensity = Intensity, angle = Angle, us_r = UsR, us_v = UsV} = _Data) ->
  generate_sample(UsR, UsV, Intensity, Angle);

generate_random_sample(ldr, #data{intensity = Intensity, angle = Angle, ldr_r = LdrR, ldr_v = LdrV} = _Data) ->
  generate_sample(LdrR, LdrV, Intensity, Angle).

generate_sample(R, Variance, Cutoff, Angle) ->
  Radius = round(rand:normal(R, Variance)),
  case (rand:uniform()) of
    Prob when Prob > Cutoff -> no_sample;
    _ when Radius < 2 -> {Angle, 2};
    _ -> {Angle, Radius}
   end.
