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

-include("defs.hrl").

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([idle/3, rec_ack/3, rec/3]).

-define(SERVER, ?MODULE).

-record(data, {rec_buf, rec_amount, rec_type, serial_port, operator_port,
              expected_ack, msg_ack, postpones}).

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
    gen_statem:start_monitor(?MODULE, Args, []).

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

-spec rec_ack('enter',
		 OldState :: atom(),
		 Data :: term()) ->
	  gen_statem:state_enter_result('state_name');
		(gen_statem:event_type(),
		 Msg :: term(),
		 Data :: term()) ->
	  gen_statem:event_handler_result(atom()).
-spec rec('enter',
		 OldState :: atom(),
		 Data :: term()) ->
	  gen_statem:state_enter_result('state_name');
		(gen_statem:event_type(),
		 Msg :: term(),
		 Data :: term()) ->
	  gen_statem:event_handler_result(atom()).
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

format_ultrasonic(RecBuf) ->
  RecBuf.

format_ldr(RecBuf) ->
  RecBuf.


