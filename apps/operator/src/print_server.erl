%%%-------------------------------------------------------------------
%%% @author Elias Assaf <elias>
%%% @copyright (C) 2023, Elias Assaf
%%% @doc
%%%
%%% @end
%%% Created: 28 June 2023
%%%-------------------------------------------------------------------
-module(print_server).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {ports}).

-export([
         send_telemeter/1,
         send_file/0,
         send_to_com/2,
         get_comm/0,
         add_comm/1
        ]).
%%%===================================================================
%%% API
%%%===================================================================

send_telemeter(Angle) when is_integer(Angle) ->
  send_to_com(telemeter, Angle).

send_file() ->
  SampleFile = <<16#010A:16, 16#041E:16, 16#0214:16, 16#05:8,
                 16#0623:16, 16#0114:16, 16#07143C:24, 16#08:8>>,
  %% SampleFile = <<16#08:8,16#08:8>>,
  send_to_com(file, SampleFile).

send_to_com(Opcode, OpcodeData) ->
  gen_statem:call(?SERVER, {send, Opcode, OpcodeData}).

get_comm() ->
  gen_statem:call(?SERVER, get_comm).

add_comm(PortFile) ->
  gen_statem:call(?SERVER, {add_comm, PortFile}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init(Args) ->
    process_flag(trap_exit, true),
    PortFiles = proplists:get_value(port_file, Args, ["/dev/ttyACM0"]),
    Ports = lists:map(fun(PortFile) ->
                  {ok, Pid} = communication:start_link([{operator, ?SERVER}, {port_file, PortFile}]),
                  {Pid, PortFile}
              end, PortFiles),
    {ok, #state{ports = Ports}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.

handle_call(get_comm, _From, State) ->
  Pids = lists:map(fun({Pid, _PortFile}) -> Pid end, State#state.ports),
  {reply, Pids, State};
handle_call(Msg = {send, Opcode, _OpcodeData}, _From, State) ->
  lists:foreach(fun({Pid, _}) -> gen_statem:cast(Pid, Msg) end, State#state.ports),
  {reply, Opcode, State};
handle_call({add_comm, PortFile}, _From, State= #state{ports = Ports}) ->
  case communication:start_link([{operator, ?SERVER}, {port_file, PortFile}]) of
    {ok, Port} ->
      {reply, ok, State#state{ports = [{Port, PortFile} | Ports]}};
    Err ->
      {reply, Err, State}
  end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_cast({ultrasonic, Port, Val}, State = #state{ports = Ports}) ->
  {_, PortFile} = lists:keyfind(Port, 1, Ports),
  io:format("got ultrasonic ~w, from ~s~n", [Val, PortFile]),
  {noreply, State};
handle_cast({ldr, Port, Val}, State = #state{ports = Ports}) ->
  {_, PortFile} = lists:keyfind(Port, 1, Ports),
  io:format("got ldr ~w, from ~s~n", [Val, PortFile]),
  {noreply, State};
handle_cast(Msg, State) ->
  io:format("got msg ~w~n", [Msg]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.
handle_info({'DOWN', _MonRef, process, Pid, Reason}, State = #state{ports = Ports}) ->
  PortFile = lists:keyfind(Pid, 1, Ports),
  io:format("process ~w, file ~w down, reason ~w~n", [Pid, PortFile, Reason]),
  case State#state{ports = lists:keydelete(Pid, 1, Ports)} of
    [] ->
      {stop, noports};
    NewState ->
      {noreply, NewState}
  end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================


