%%%-------------------------------------------------------------------
%%% @author Elias Assaf <elias>
%%% @copyright (C) 2023, Elias Assaf
%%% @doc
%%%
%%% @end
%%% Created: 18 June 2023
%%%-------------------------------------------------------------------
-module(radar).

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").

%% API
-export([start_link/0]).

%% wx_object callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_event/2, terminate/2, code_change/3]).

%% internal usage
-export([advance_uptime/0]).

-define(WXSERVER, ?MODULE).

-record(stats, {
          uptime = 0,
          rec_msg = 0,
          num_nodes = 0,
          num_radars = 0
}).

-record(state, {
          frame,
          canvas,
          status_bar,
          status_bar_stats
}).

-define(SUS_BUTTON, 100).
-define(SLDR_BUTTON, 101).
-define(SDUAL_BUTTON, 102).
-define(FILE1_BUTTON, 103).
-define(FILE2_BUTTON, 104).
-define(FILE3_BUTTON, 105).
-define(STATS_BUTTON, 106).
-define(SFILE_BUTTON, 107).
-define(STELEM_BUTTON, 108).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> wxWindow()
%% @end
%%--------------------------------------------------------------------
start_link() ->
  wx_object:start_link({local, ?WXSERVER}, ?MODULE, [], []).

%%%===================================================================
%%% wx_object callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {wxWindow(), State} |
%%                     {wxWindow(), State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
%%
%%  ----MainSizer vertical----
%%    ----Status bar----
%%    ------Canvas------
%%    |                |
%%    |                |
%%    |                |
%%    ------------------
%%    -Button grid sizer-
%%    | butt1    butt2 |
%%    | butt3    butt4 |
%%    ------------------
%%
%%
init([]) ->
  wx:new(),
  Frame = wxFrame:new(wx:null(), 1 , "Radar"),
  % spawn windows
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  StatusBar = wxStatusBar:new(Frame),
  wxStatusBar:setFieldsCount(StatusBar, 3, [{widths, [100, 200, 100]}]),
  wxStatusBar:setStatusText(StatusBar, "Uptime: 0:00", [{number, 0}]),
  wxStatusBar:setStatusText(StatusBar, "Nodes/Radars connected: 0/0", [{number, 1}]),
  Canvas = wxPanel:new(Frame, [{size, {500, 500}}, {style, ?wxBORDER_SIMPLE}]),
  wxPanel:setBackgroundColour(Canvas, ?wxWHITE),
  Font = wxFont:new(8, ?wxFONTFAMILY_MODERN, ?wxFONTSTYLE_NORMAL, ?
                    wxFONTWEIGHT_BOLD),
  wxStatusBar:setFont(StatusBar, Font),
  ButtonGridSizer = wxGridSizer:new(3, 3, 2, 2), % rows, cols, vgap, hgap

  ScanUsButton = wxButton:new(Frame, ?SUS_BUTTON, [{label, "Scan US"}]),
  wxGridSizer:add(ButtonGridSizer, ScanUsButton,
                  [{proportion, 0}, {flag, ?wxALIGN_TOP bor ?wxALIGN_LEFT}]),

  ScanLdrButton = wxButton:new(Frame, ?SLDR_BUTTON, [{label, "Scan LDR"}]),
  wxGridSizer:add(ButtonGridSizer, ScanLdrButton,
                  [{proportion, 0}, {flag, ?wxALIGN_TOP bor ?wxALIGN_CENTER_HORIZONTAL}]),

  DualScanButton = wxButton:new(Frame, ?SDUAL_BUTTON, [{label, "Dual Scan"}]),
  wxGridSizer:add(ButtonGridSizer, DualScanButton,
                  [{proportion, 0}, {flag, ?wxALIGN_TOP bor ?wxALIGN_RIGHT}]),

  File1Button = wxButton:new(Frame, ?FILE1_BUTTON, [{label, "Start file 1"}]),
  wxGridSizer:add(ButtonGridSizer, File1Button,
                  [{proportion, 0}, {flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_LEFT}]),

  File2Button = wxButton:new(Frame, ?FILE2_BUTTON, [{label, "Start file 2"}]),
  wxGridSizer:add(ButtonGridSizer, File2Button,
                  [{proportion, 0}, {flag, ?wxALIGN_CENTER}]),

  File3Button = wxButton:new(Frame, ?FILE3_BUTTON, [{label, "Start file 3"}]),
  wxGridSizer:add(ButtonGridSizer, File3Button,
                  [{proportion, 0}, {flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_RIGHT}]),

  ShowStatsButton = wxButton:new(Frame, ?STATS_BUTTON, [{label, "Show Stats"}]),
  wxGridSizer:add(ButtonGridSizer, ShowStatsButton,
                  [{proportion, 0}, {flag, ?wxALIGN_BOTTOM bor ?wxALIGN_LEFT}]),

  SendFileButton = wxButton:new(Frame, ?SFILE_BUTTON, [{label, "Send File"}]),
  wxGridSizer:add(ButtonGridSizer, SendFileButton,
                  [{proportion, 0}, {flag, ?wxALIGN_BOTTOM bor ?wxALIGN_CENTER}]),

  SendTelemer = wxButton:new(Frame, ?STELEM_BUTTON, [{label, "Scan Angle"}]),
  wxGridSizer:add(ButtonGridSizer, SendTelemer,
                  [{proportion, 0}, {flag, ?wxALIGN_BOTTOM bor ?wxALIGN_RIGHT}]),

  wxBoxSizer:add(MainSizer, StatusBar, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxBoxSizer:add(MainSizer, Canvas, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxBoxSizer:add(MainSizer, ButtonGridSizer, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),

  wxWindow:setSizer(Frame, MainSizer),
  wxSizer:setSizeHints(MainSizer, Frame),

  % connect windows to events
  wxFrame:connect(Frame, close_window),
  wxFrame:connect(Frame, command_button_clicked),
  timer:apply_interval(1000, ?MODULE, advance_uptime, []),
  wxFrame:show(Frame),
  {Frame, #state{frame = Frame, canvas = Canvas, status_bar = StatusBar, status_bar_stats = #stats{}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling events
%%
%% @spec handle_event(wx{}, State) ->
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_event(#wx{event = #wxClose{}}, State) ->
  {stop, normal, State};

handle_event(#wx{id=?SUS_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State = #state{}) ->
  % do something with the button
  io:format("Scan button pressed~n"),
  {noreply,State};

handle_event(#wx{id=?SLDR_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State = #state{}) ->
  {noreply,State};

handle_event(#wx{id=?SDUAL_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State = #state{}) ->
  {noreply,State};

handle_event(#wx{id=?FILE1_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State = #state{}) ->
  {noreply,State};

handle_event(#wx{id=?FILE2_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State = #state{}) ->
  {noreply,State};

handle_event(#wx{id=?FILE3_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State = #state{}) ->

  {noreply,State};

handle_event(#wx{id=?STATS_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State = #state{frame = Frame, status_bar_stats = Stats}) ->
  Env = wx:get_env(),
  spawn(fun() -> stats_dialog(Env, Stats) end),
  {noreply,State};

handle_event(#wx{id=?SFILE_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State = #state{frame = _Frame}) ->
  Env = wx:get_env(),
  spawn(fun() -> send_file_dialog(Env) end),
  {noreply,State};

handle_event(#wx{id=?STELEM_BUTTON, event=#wxCommand{type=command_button_clicked}},
             State = #state{frame = _Frame}) ->
  Env = wx:get_env(),
  spawn(fun() -> slider_dialog(Env) end),
  {noreply,State};

handle_event(Cmd = #wx{}, State) ->
  io:format("got event: ~w~n", [Cmd]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  io:format("~w~n", [_Request]),
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({advance_uptime}, State) ->
  io:format("ADVANCEEE!!~n"),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({new_frame}, State) ->
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a wx_object when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the wx_object terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
  radar_app:stop(State),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

slider_dialog(Env) ->
  wx:set_env(Env),
  SliderDialog = wxDialog:new(wx:null(), ?wxID_ANY, "Set Radar Angle", [
     {style, ?wxDEFAULT_DIALOG_STYLE}
    ]),
  DialogSizer = wxBoxSizer:new(?wxVERTICAL),

  Buttons = wxDialog:createButtonSizer(SliderDialog, ?wxOK bor ?wxCANCEL),
  Slider = wxSlider:new(SliderDialog, ?wxID_ANY, 90, 0, 180, [
     {style, ?wxSL_HORIZONTAL bor ?wxSL_LABELS bor ?wxSL_BOTTOM}
  ]),
  wxSizer:add(DialogSizer, Slider, [
     {flag, ?wxEXPAND bor ?wxALIGN_CENTER bor ?wxALL},
     {border, 5}
  ]),
  wxSizer:add(DialogSizer, Buttons, [
     {flag, ?wxEXPAND bor ?wxALIGN_CENTER bor ?wxALL},
     {border, 5}
  ]),

  wxDialog:setSizer(SliderDialog, DialogSizer),
  wxSizer:setSizeHints(DialogSizer, SliderDialog),

  case wxDialog:showModal(SliderDialog) of
    ?wxID_OK ->
      Angle = wxSlider:getValue(Slider),
      % TODO add callback
      io:format("users selected angle: ~w~n", [Angle]);
    ?wxID_CANCEL ->
      io:format("User Canceled~n")
  end,
  wxDialog:destroy(SliderDialog).

send_file_dialog(Env) ->
  wx:set_env(Env),
  {ok, CurrDir} = file:get_cwd(),
  FileDialog = wxFileDialog:new(wx:null(),[
    {message, "Pick a file to send"},
    {style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST bor ?wxFD_PREVIEW},
    {defaultDir, CurrDir},
    {defaultFile, ""}
  ]),
  case wxFileDialog:showModal(FileDialog) of
    ?wxID_OK ->
      % TODO implement functionality
      FilePath = wxFileDialog:getPath(FileDialog),
      io:format("user clicked ~s~n", [FilePath]);
    ?wxID_CANCEL ->
      io:format("user canceled~n")
  end,
  wxFileDialog:destroy(FileDialog).

stats_dialog(Env, Stats) ->
  wx:set_env(Env),
  StatsDialog = wxDialog:new(wx:null(), ?wxID_ANY,"Stats Report", [
    {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER}
   ]),
 StatsSizer = wxBoxSizer:new(?wxVERTICAL),
 Buttons = wxDialog:createButtonSizer(StatsDialog, ?wxOK),

 Font = wxFont:new(9, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?
 wxFONTWEIGHT_BOLD),
 wxListCtrl:setFont(StatsDialog, Font),

 ListCtrl = wxListCtrl:new(StatsDialog, [
   {style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL bor ?wxLC_VRULES}
 ]),
 wxListCtrl:insertColumn(ListCtrl, 0, "Statistic", []),
 wxListCtrl:setColumnWidth(ListCtrl, 0, 100),
 wxListCtrl:insertColumn(ListCtrl, 1, "Value", []),
 wxListCtrl:setColumnWidth(ListCtrl, 1, 100),
 Fun =
  fun({Idx, Name, Value}) ->
     ValStr = integer_to_list(Value),
     wxListCtrl:insertItem(ListCtrl, Idx, ""),
     wxListCtrl:setItem(ListCtrl, Idx, 0, atom_to_list(Name)),
     wxListCtrl:setItem(ListCtrl, Idx, 1, ValStr),
     case (Idx rem 2) of
       0 ->
         ok;
       1 ->
         wxListCtrl:setItemBackgroundColour(ListCtrl, Idx, {240,240,240,255})
     end,
     ok
  end,
 Names = record_info(fields, stats),
 [_| Values] = tuple_to_list(Stats),
 wx:foreach(Fun, lists:zip3(lists:seq(0, length(Names) - 1), Names, Values)),

 wxBoxSizer:add(StatsSizer, ListCtrl, [
   {flag, ?wxEXPAND bor ?wxALIGN_CENTER bor ?wxALL},
   {border, 5}
 ]),
 
 wxBoxSizer:add(StatsSizer, Buttons, [
   {flag, ?wxALIGN_CENTER bor ?wxALL},
   {border, 5}
 ]),
 % wxBoxSizer:setMinSize(StatsSizer, 300, 300),
 wxDialog:setSizer(StatsDialog, StatsSizer),
 wxSizer:setSizeHints(StatsSizer, StatsDialog),

 wxDialog:showModal(StatsDialog),
 wxDialog:destroy(StatsDialog).

schedule_frames() ->
  timer:send_interval(40, ?WXSERVER, {new_frame}).


advance_uptime() ->
  wx_object:cast(?WXSERVER, {advance_uptime}).


