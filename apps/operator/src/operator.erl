%%%-------------------------------------------------------------------
%%% @author Elias Assaf <elias>
%%% @copyright (C) 2023, Elias Assaf
%%% @doc
%%%
%%% @end
%%% Created: 18 June 2023
%%%-------------------------------------------------------------------
-module(operator).

-behaviour(gen_server).

-include("inotify/include/inotify.hrl").

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

%% inotify event
-export([inotify_event/3]).

-define(SERVER, ?MODULE).

-record(state, {comm_map, inotify_ref}).

-record(comm_info, {atom_name}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% start_link/0 is used for starting the server as a regular gen_server
%% start_link/1 is used to start operator as an application in a
%% supervision tree.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.
start_link() ->
  case application:ensure_started(inotify, permanent) of
    ok ->
      gen_server:start_link({local, ?SERVER}, ?MODULE, [], []);
    Err ->
      Err
  end.

-spec start_link(supervisor) ->
  {ok, Pid :: pid()} |
  {error, Error :: {already_started, pid()}} |
  {error, Error :: term()} |
  ignore.

start_link(supervisor) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
  process_flag(trap_exit, true),
  case filelib:ensure_dir("/dev/serial/by-id") of
    {error, _Err} ->
      init_dev();
    ok ->
      init_serial()
  end.

init_dev() ->
  Ref = inotify:watch("/dev", [create]),
  inotify:add_handler(Ref, ?SERVER, dev),
  {ok, #state{comm_map = #{}, inotify_ref = Ref}}.

init_serial() ->
  CommMap = get_all_comms(),
  Ref = inotify:watch("/dev/serial/by-id", [create]),
  inotify:add_handler(Ref, ?SERVER, ser),
  {ok, #state{comm_map = CommMap, inotify_ref = Ref}}.

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

handle_cast({inotify, ser, _EventTag, _Masks, Name}, #state{comm_map = CommMap} = State) ->
  case get_comm(Name) of
    {true, {Cid, CommInfo}} ->
      % TODO send to radar new one connected
      {noreply, State#state{comm_map = CommMap#{Cid => CommInfo}}};
    false ->
      {noreply, State}
  end;

handle_cast({inotify, dev, _EventTag, _Masks, "serial"}, #state{inotify_ref = OldRef} = State) ->
  CommMap = get_all_comms(),
  % TODO send to radar new one connected
  inotify:unwatch(OldRef),
  Ref = inotify:watch("/dev/serial/by-id/", [create]),
  inotify:add_handler(Ref, ?SERVER, ser),
  {noreply, State#state{comm_map = CommMap, inotify_ref = Ref}};

handle_cast(_Request, State) ->
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

handle_info({'EXIT', Pid, normal}, #state{comm_map = CommMap, inotify_ref = OldRef} = State) ->
  case maps:take(Pid, CommMap) of
    {_Name, NewMap} ->
      % TODO send to radar that name/Pid was removed
      Ref = case map_size(NewMap) of
        0 ->
          inotify:unwatch(OldRef),
          NewRef = inotify:watch("/dev", [create]),
          inotify:add_handler(NewRef, ?SERVER, dev),
          NewRef;
        _ ->
          OldRef
      end,
      {noreply, State#state{comm_map = NewMap, inotify_ref = Ref}};
    error ->
      %% who died?
      {noreply, State}
  end;
handle_info(_Info, State) ->
  io:format("~w~n", [_Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling inotify events
%% @end
%%--------------------------------------------------------------------
-spec inotify_event(Arg :: term(), EventTag :: reference(), MsgContents :: '?inotify_msg') ->
  ok.

inotify_event(Arg, EventTag, ?inotify_msg(Masks, _Cookie, Filename)) ->
  gen_server:cast(?SERVER, {inotify, Arg, EventTag, Masks, Filename});

inotify_event(Arg, _EventTag, ?inotify_msg(Masks, _Cookie, Name)) ->
  io:format("unknown inotify event: Masks: ~p, Name: ~p, Arg ~p~n", [Masks, Name, Arg]).

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

get_all_comms() ->
  % delay since it takes a bit of time to create the by-id dir
  {ok, _Dirs} = file:list_dir("/dev/serial/"),
  {ok, Filenames} = file:list_dir("/dev/serial/by-id/"),
  Comms = lists:filtermap(fun get_comm/1, Filenames),
  CommMap = maps:from_list(Comms),
  CommMap.

get_comm(Name) ->
  RegExp = "usb-Texas_Instruments_Texas_Instruments_MSP-FET430UIF_[0-9A-Z]+-if00",
  case re:run(Name, RegExp) of
    {match, [{F, L} | _]} ->
      Filename = string:sub_string(Name, F+1, F+L),
      {ok, Cid} = communication:start_link([{port_file, "/dev/serial/by-id/" ++ Filename}]),
      {true, {Cid, #comm_info{atom_name = list_to_atom(Filename)}}};
    nomatch ->
      false
  end.

