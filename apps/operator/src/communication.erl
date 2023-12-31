%%%-------------------------------------------------------------------
%%% @author Elias Assaf <elias>
%%% @copyright (C) 2023, Elias Assaf
%%% @doc
%%%
%%% @end
%%% Created: 18 June 2023
%%%-------------------------------------------------------------------
-module(communication).

-behaviour(gen_statem).

-include("include/defs.hrl").

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([idle/3, rec_ack/3, rec/3]).

-define(SERVER, ?MODULE).

-record(data, {rec_buf, rec_amount, rec_type, serial_port, operator_port,
              expected_ack, postpones, file}).
-define(TIMEOUT_TIME, 5000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% user exposed API's
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Args :: term()) ->
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
callback_mode() -> state_functions.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
  gen_statem:init_result(atom()).
init([]) ->
  {error, no_port_given};
init(Args) ->
  process_flag(trap_exit, true),
  case proplists:get_value(port_file, Args) of
    undefined ->
      {error, no_port_given};
    PortFile ->
      Speed = proplists:get_value(speed, Args, 9600),
      OperatorPort = proplists:get_value(operator, Args, operator),
      SerialPort = serial:start([{open, PortFile}, {speed, Speed}]),
      {ok, idle, #data{operator_port = OperatorPort, serial_port = SerialPort}}
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one function like this for each state name.
%% Whenever a gen_statem receives an event, the function
%% with the name of the current state (StateName)
%% is called to handle the event.
%% @end
%%--------------------------------------------------------------------
-spec idle('enter',
           OldState :: atom(),
           Data :: term()) ->
  gen_statem:state_enter_result('state_name');
          (gen_statem:event_type(),
           Msg :: term(),
           Data :: term()) ->
  gen_statem:event_handler_result(atom()).

% ultrasonic
idle(info, {data, <<?MSPPC_ULTRASONIC:2, Degree:6>>}, Data) ->
  {next_state, rec, Data#data{rec_type = ultrasonic, rec_amount = 2, rec_buf = <<Degree:8>>}, ?TIMEOUT_TIME};
% ldr
idle(info, {data, <<?MSPPC_LDR:2, Degree:6>>}, Data) ->
  {next_state, rec, Data#data{rec_type = ldr, rec_amount = 1, rec_buf = <<Degree:8>>}, ?TIMEOUT_TIME};
idle(info, {data, _Byte = <<_Two:2, _Arg:6>>}, _Data) ->
  %% io:format("got unexpected data from msp430: ~w = <<~w:2,~w:6>>~n", [_Byte, _Two, _Arg]),
  keep_state_and_data;
% commands to send
idle(cast, {send, telemeter, Angle}, Data) ->
  Data#data.serial_port ! {send, <<?PCMSP_TELEMETER:2, (Angle div 3):6>>},
  {next_state, rec_ack, Data#data{expected_ack = ?PCMSP_TELEMETER, postpones = 0}, {state_timeout, ?TIMEOUT_TIME, Data}};
idle(cast, {send, file, File}, Data) ->
  FileSize = byte_size(File),
  Data#data.serial_port ! {send, <<?PCMSP_FILE:2, FileSize:6>>},
  {next_state, rec_ack, Data#data{expected_ack = ?PCMSP_FILE, postpones = 0, file = File, rec_amount = FileSize}, {state_timeout, ?TIMEOUT_TIME, Data}};
idle(cast, {send, Opcode, _OpData}, Data) ->
  Data#data.serial_port ! {send, <<?PCMSP_COMMAND:2, Opcode:6>>},
  {next_state, rec_ack, Data#data{expected_ack = Opcode, postpones = 0}, {state_timeout, ?TIMEOUT_TIME, Data}};
idle(info, {'EXIT', _PID, _Reason}, _Data) ->
  {stop, _Reason};
% catch all
idle(_Type, _Msg, _Data) ->
  keep_state_and_data.

-spec rec_ack('enter',
              OldState :: atom(),
              Data :: term()) ->
  gen_statem:state_enter_result('state_name');
             (gen_statem:event_type(),
              Msg :: term(),
              Data :: term()) ->
  gen_statem:event_handler_result(atom()).

% postpone messages from MSP and cmds from operator until ack arrives, or we timeout
% when sending file we send byte byte and wait for ack each time
rec_ack(info, {data, _Byte}, #data{postpones = Postpones} = Data) when Postpones =/= 0 ->
  {next_state, rec_ack, Data#data{postpones = Postpones-1}, postpone};
rec_ack(info, {data, <<?MSPPC_ULTRASONIC:2, _Ack:6>>}, Data) ->
  {next_state, rec_ack, Data#data{postpones = 2}, postpone};
rec_ack(info, {data, <<?MSPPC_LDR:2, _Ack:6>>}, Data) ->
  {next_state, rec_ack, Data#data{postpones = 1}, postpone};
rec_ack(info, {data, <<?MSPPC_ACK:2, Ack:6>>}, #data{expected_ack = Ack, rec_amount = 0} = Data) when Ack =:= ?PCMSP_FILE ->
  gen_server:cast(Data#data.operator_port, {ack, self(), file_recevied}),
  {next_state, idle, Data};
rec_ack(info, {data, <<?MSPPC_ACK:2, Ack:6>>}, #data{file = <<H:8, Tail/binary>>, expected_ack = Ack, rec_amount = RecAmount} = Data) when Ack =:= ?PCMSP_FILE ->
  Data#data.serial_port ! {send, <<H:8>>},
  gen_server:cast(Data#data.operator_port, {ack, self(), {ready_to_rec_next, RecAmount}}),
  % what type of ack should we expect?
  {next_state, rec_ack, Data#data{file = Tail, rec_amount = RecAmount-1}, {state_timeout, ?TIMEOUT_TIME, Data}};
rec_ack(info, {data, <<?MSPPC_ACK:2, Ack:6>>}, #data{expected_ack = Ack} = Data) ->
  gen_server:cast(Data#data.operator_port, {ack, self(), Ack}),
  {next_state, idle, Data};
rec_ack(info, {data, <<?MSPPC_ACK:2, _Ack1:6>>}, Data = #data{expected_ack = _Ack2}) ->
  CMD = ?IDLE_CMD,
  Data#data.serial_port ! {send, <<?PCMSP_COMMAND:2, CMD:6>>},
  % what should we do here?
  gen_server:cast(Data#data.operator_port, {wrong_ack, self(), _Ack1, _Ack2}),
  {next_state, rec_ack, Data#data{expected_ack = CMD}, {state_timeout, ?TIMEOUT_TIME, Data}};
rec_ack(info, {'EXIT', _PID, _Reason}, _Data) ->
  {stop, _Reason};
rec_ack(cast, _Msg, Data) ->
  {next_state, rec_ack, Data, postpone};
rec_ack(state_timeout, _Msg, Data) ->
  {next_state, idle, Data};
% catch all
rec_ack(_Type, _Msg, _Data) ->
  keep_state_and_data.

-spec rec('enter',
          OldState :: atom(),
          Data :: term()) ->
  gen_statem:state_enter_result('state_name');
         (gen_statem:event_type(),
          Msg :: term(),
          Data :: term()) ->
  gen_statem:event_handler_result(atom()).

% depending on receive type (set from idle state) recieve specific amount of bytes,
% then call formatter and send result to operator.
rec(info, {data, Byte}, #data{rec_type = ultrasonic, rec_amount = 1} = Data) ->
  % send data to upper layer, goto idle
  RecBuf = <<(Data#data.rec_buf)/binary, Byte/binary>>,
  case format_ultrasonic(RecBuf) of
    {_Angle, Dist} = Sample when Dist > 0 andalso Dist < 400 ->
      gen_server:cast(Data#data.operator_port, {ultrasonic, self(), Sample});
    _ -> ok
  end,
  {next_state, idle, Data};
rec(info, {data, Byte}, #data{rec_type = ultrasonic} = Data) ->
  %append Byte to rec buffer
  RecAmount = Data#data.rec_amount - 1,
  RecBuf = <<(Data#data.rec_buf)/binary, Byte/binary>>,
  {next_state, rec, Data#data{rec_amount = RecAmount, rec_buf = RecBuf}, ?TIMEOUT_TIME};
rec(info, {data, Byte}, #data{rec_type = ldr, rec_amount = 1} = Data) ->
  % send data to upper layer, goto idle
  RecBuf = <<(Data#data.rec_buf)/binary, Byte/binary>>,
  case format_ldr(RecBuf) of
    {_Angle, Dist} = Sample when Dist >= 0 andalso Dist =< 50 ->
      %% io:format("ldr dist: ~p~n", [Dist]),
      gen_server:cast(Data#data.operator_port, {ldr, self(), Sample});
    _ -> ok
  end,
  {next_state, idle, Data};
rec(cast, _Msg, Data) ->
  {next_state, rec, Data, [postpone, 5000]};
rec(timeout, _Msg, _Data) ->
  {stop, timeout};
rec(info, {'EXIT', _Pid, _Reason}, _Data) ->
  {stop, _Reason};
% catch all
rec(_Type, _Msg, _Data) ->
  keep_state_and_data.

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
        OldVsn :: term() | {down, term()},
        State :: term(), Data :: term(), Extra :: term()) ->
  {ok, NewState :: term(), NewData :: term()} |
  (Reason :: term()).
code_change(_OldVsn, State, Data, _Extra) ->
  {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

format_ultrasonic(<<Deg:8, Time:16>>) ->
  {Deg*3, (Time * 17000) div 1048576}.

format_ldr(<<Deg:8, Dist:8>>) ->
  {Deg*3, Dist}.


