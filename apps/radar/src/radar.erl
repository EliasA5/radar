%%%-------------------------------------------------------------------
%%% @author Elias Assaf <elias>
%%% @copyright (C) 2023, Elias Assaf
%%% @doc
%%%
%%% @end
%%% Created: 03 July 2023
%%%-------------------------------------------------------------------
-module(radar).

-behaviour(gen_server).

-include_lib("wx/include/wx.hrl").

-include("include/defs.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2, code_change/3, format_status/2]).

-export([connect_radar/2, disconnect_radar/2, reconnect_operator/1]).

%% internal usage
-export([]).

-define(SERVER, ?MODULE).

-record(stats, {
          uptime = 0,
          rec_msg = 0,
          num_nodes = 0,
          num_radars = 0,
          active_detections = 0
}).

-record(state, {
          frame,
          canvas,
          background,
          detections_bar,
          status_bar,
          status_bar_stats,
          click_info,
          radars,
          single_sample = false,
          radar_backup,
          noti_box,
          chosen_file = {"~", ""}
}).

-record(click_info, {
          key,
          offset = {0, 0},
          selected
}).

-record(radar_info, {
          pos,
          angle = 0,
          overlay,
          bitmap,
          node,
          pid,
          name,
          samples = []
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
-define(BACKGROUND_BUTTON, 109).
-define(IDLE_BUTTON, 110).
-define(EMPTY_BUTTON, 111).

-define(RADAR_DRAWING, "imgs/radar-normal.png").
-define(RADAR_DRAWING_SELECTED, "imgs/radar-selected.png").
-define(DETECTIONS_DRAWING, "imgs/detections.png").
-define(DEFAULT_BACKGROUND_DRAWING, "background.png").

-define(BITMAP_WIDTH, 25).
-define(BITMAP_HEIGHT, 20).
-define(DETECTION_EDGE, 20).

%%%===================================================================
%%% API
%%%===================================================================

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
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Connects a new radar with Pid from Node
%% Info is a map that includes:
%% Info#{pid := Pid, name := Name, something_else => SE}
%% where := is must include, => is optional
%% @end
%%--------------------------------------------------------------------
%%
%%
-spec connect_radar(Node :: node(), Info :: map()) -> ok.

connect_radar(nonode@nohost, _Info) ->
  ok;
connect_radar(Node, Info) ->
  gen_server:call({global, ?SERVER}, {connect_radar, Node, Info}).

%%--------------------------------------------------------------------
%% @doc
%% Disconnects a new radar with Pid from Node
%% Info is a map that includes:
%% Info#{pid := Pid, name := Name, something_else => SE}
%% where := is must include, => is optional
%% @end
%%--------------------------------------------------------------------
-spec disconnect_radar(Node :: node(), Info :: map()) -> ok.

disconnect_radar(nonode@nohost, _Info) ->
  ok;
disconnect_radar(Node, Info) ->
  gen_server:call({global, ?SERVER}, {disconnect_radar, Node, Info}).

%%--------------------------------------------------------------------
%% @doc
%% Removes all radars that come from node Node
%% Used for when the operator crashes, and needs to resend all the
%% open communication ports
%% @end
%%--------------------------------------------------------------------
-spec reconnect_operator(Node :: node()) -> ok.

reconnect_operator(nonode@nohost) ->
  ok;
reconnect_operator(Node) ->
  gen_server:call({global, ?SERVER}, {reconnect_operator, Node}).

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
%%  ---------------MainSizer vertical-----------
%%    ----------------Status bar-------------
%%      ----------------Canvas--------------
%%    |                                        |
%%    |                                        |
%%    |                                        |
%%    ------------------------------------------
%%    -Button grid sizer---Notifications Panel--
%%                         ____________________
%%    | butt1    butt2 |  |                    |
%%    | butt3    butt4 |  |____________________|
%%    ------------------------------------------
%%
%%
init([]) ->
  process_flag(trap_exit, true),
  net_kernel:monitor_nodes(true),
  wx:new(),
  Frame = wxFrame:new(wx:null(), 1, "Radar"),
  % spawn windows
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  TopSizer = wxBoxSizer:new(?wxHORIZONTAL),
  StatusBar = wxStatusBar:new(Frame),
  wxStatusBar:setFieldsCount(StatusBar, 2, [{widths, [-3, -5]}]),
  wxStatusBar:setStatusText(StatusBar, "Uptime: 00:00:00", [{number, 0}]),
  DetectionsPanel = wxPanel:new(Frame, []),
  DetectionsText = wxStatusBar:new(Frame, []),
  DetectionsBmp = get_image_bitmap(?DETECTIONS_DRAWING, 0, ?DETECTION_EDGE, ?DETECTION_EDGE),
  wxStaticBitmap:new(DetectionsPanel, -1, DetectionsBmp),
  Canvas = wxPanel:new(Frame, [{size, {1040, 650}}, {style, ?wxBORDER_SIMPLE}]),
  Font = wxFont:new(9, ?wxFONTFAMILY_MODERN, ?wxFONTSTYLE_NORMAL, ?
                    wxFONTWEIGHT_BOLD),
  wxStatusBar:setFont(StatusBar, Font),
  wxStatusBar:setFont(DetectionsText, Font),
  wxFont:destroy(Font),
  BottomSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ButtonGridSizer = wxGridSizer:new(3, 4, 2, 2), % rows, cols, vgap, hgap

  ScanUsButton = wxButton:new(Frame, ?SUS_BUTTON, [{label, "Scan US"}]),
  wxGridSizer:add(ButtonGridSizer, ScanUsButton,
                  [{proportion, 0}, {flag, ?wxALIGN_TOP bor ?wxALIGN_LEFT bor ?wxEXPAND}]),

  ScanLdrButton = wxButton:new(Frame, ?SLDR_BUTTON, [{label, "Scan LDR"}]),
  wxGridSizer:add(ButtonGridSizer, ScanLdrButton,
                  [{proportion, 0}, {flag, ?wxALIGN_TOP bor ?wxALIGN_CENTER_HORIZONTAL bor ?wxEXPAND}]),

  DualScanButton = wxButton:new(Frame, ?SDUAL_BUTTON, [{label, "Dual Scan"}]),
  wxGridSizer:add(ButtonGridSizer, DualScanButton,
                  [{proportion, 0}, {flag, ?wxALIGN_TOP bor ?wxALIGN_RIGHT bor ?wxEXPAND}]),

  IdleButton = wxButton:new(Frame, ?IDLE_BUTTON, [{label, "Idle"}]),
  wxGridSizer:add(ButtonGridSizer, IdleButton,
                  [{proportion, 0}, {flag, ?wxALIGN_TOP bor ?wxALIGN_RIGHT bor ?wxEXPAND}]),

  File1Button = wxButton:new(Frame, ?FILE1_BUTTON, [{label, "Start file 1"}]),
  wxGridSizer:add(ButtonGridSizer, File1Button,
                  [{proportion, 0}, {flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_LEFT bor ?wxEXPAND}]),

  File2Button = wxButton:new(Frame, ?FILE2_BUTTON, [{label, "Start file 2"}]),
  wxGridSizer:add(ButtonGridSizer, File2Button,
                  [{proportion, 0}, {flag, ?wxALIGN_CENTER bor ?wxEXPAND}]),

  File3Button = wxButton:new(Frame, ?FILE3_BUTTON, [{label, "Start file 3"}]),
  wxGridSizer:add(ButtonGridSizer, File3Button,
                  [{proportion, 0}, {flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_RIGHT bor ?wxEXPAND}]),

  EmptyButton = wxButton:new(Frame, ?EMPTY_BUTTON, [{label, "Empty"}]),
  wxGridSizer:add(ButtonGridSizer, EmptyButton,
                  [{proportion, 0}, {flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_RIGHT bor ?wxEXPAND}]),

  ShowStatsButton = wxButton:new(Frame, ?STATS_BUTTON, [{label, "Show Stats"}]),
  wxGridSizer:add(ButtonGridSizer, ShowStatsButton,
                  [{proportion, 0}, {flag, ?wxALIGN_BOTTOM bor ?wxALIGN_LEFT bor ?wxEXPAND}]),

  SendFileButton = wxButton:new(Frame, ?SFILE_BUTTON, [{label, "Send File"}]),
  wxGridSizer:add(ButtonGridSizer, SendFileButton,
                  [{proportion, 0}, {flag, ?wxALIGN_BOTTOM bor ?wxEXPAND}]),

  SendTelemer = wxButton:new(Frame, ?STELEM_BUTTON, [{label, "Scan Angle"}]),
  wxGridSizer:add(ButtonGridSizer, SendTelemer,
                  [{proportion, 0}, {flag, ?wxALIGN_BOTTOM bor ?wxALIGN_RIGHT bor ?wxEXPAND}]),

  BackgroundButton = wxButton:new(Frame, ?BACKGROUND_BUTTON, [{label, "Background"}]),
  wxGridSizer:add(ButtonGridSizer, BackgroundButton,
                  [{proportion, 0}, {flag, ?wxALIGN_BOTTOM bor ?wxALIGN_RIGHT bor ?wxEXPAND}]),

  NotificationsBox = wxTextCtrl:new(Frame, ?wxID_ANY, [
                  {style, ?wxTE_BESTWRAP bor ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_LEFT},
                  {size, {450, 100}}
                ]),

  wxBoxSizer:add(BottomSizer, ButtonGridSizer, [
                  {flag, ?wxALL bor ?wxALIGN_LEFT},
                  {border, 5}
                 ]),
  wxBoxSizer:add(BottomSizer, NotificationsBox, [
                   {flag, ?wxALL bor ?wxALIGN_LEFT},
                   {border, 5}
                 ]),
  wxBoxSizer:add(TopSizer, StatusBar, [{proportion, 4}, {flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxBoxSizer:add(TopSizer, DetectionsPanel, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxBoxSizer:add(TopSizer, DetectionsText, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxBoxSizer:add(MainSizer, TopSizer, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxBoxSizer:add(MainSizer, Canvas, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxBoxSizer:add(MainSizer, BottomSizer, [{flag, ?wxALL bor ?wxALIGN_CENTRE}, {border, 5}]),
  wxWindow:setSizer(Frame, MainSizer),
  wxSizer:setSizeHints(MainSizer, Frame),

  % connect windows to events
  wxFrame:connect(Frame, close_window),
  wxFrame:connect(Frame, command_button_clicked),
  wxFrame:connect(Frame, iconize),

  wxPanel:connect(Canvas, paint, [callback]),
  wxPanel:connect(Canvas, size),
  wxPanel:connect(Canvas, left_down),
  wxPanel:connect(Canvas, right_down),
  wxPanel:connect(Canvas, middle_down),
  {ok, _TRef} = timer:send_interval(1000, {advance_uptime}),
  Icon = wxIcon:new("imgs/app_icon.png"),
  wxFrame:setIcon(Frame, Icon),
  wxIcon:destroy(Icon),
  wxFrame:show(Frame),
  {W, H} = wxPanel:getSize(Canvas),
  BackgroundBitmap = get_image_bitmap("backgrounds/" ++ ?DEFAULT_BACKGROUND_DRAWING, 0, W, H),
  BackgroundOverlay = wxOverlay:new(),
  Background = {BackgroundOverlay, BackgroundBitmap, "backgrounds/" ++ ?DEFAULT_BACKGROUND_DRAWING},

  {ok, RadarBackup} = dets:open_file(radar_backup, [{auto_save, 60000}, {ram_file, true}]),
  {ok, #state{frame = Frame, canvas = Canvas, radar_backup = RadarBackup, detections_bar = DetectionsText,
              background = Background, noti_box = NotificationsBox, status_bar = StatusBar,
              status_bar_stats = #stats{}, click_info = #click_info{selected = sets:new()}, radars = #{}},
       {continue, [redraw_stat_bar, redraw_detections_bar, load_background, redraw_background,
       {log, "~s, radar app starting~n", [get_current_time_str(calendar)]}]}
  }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling gui events
%% @end
%%--------------------------------------------------------------------
-spec handle_event(Request :: #wx{}, State :: term()) ->
  {noreply, NewState :: term()} |
  {noreply, NewState :: term(), Timeout :: timeout()} |
  {stop, Reason :: term(), NewState :: term()}.

%% Mouse Events
handle_event(#wx{event = #wxMouse{type=middle_down, x=X, y=Y}},
             #state{radars = Radars} = State) ->
  Bmp = get_image_bitmap(?RADAR_DRAWING),
  Pos = reclip(X, Y, wxPanel:getSize(State#state.canvas)),
  Overlay = wxOverlay:new(),
  NewRadars = Radars#{X => #radar_info{overlay = Overlay, pos = Pos, bitmap = Bmp}},
  {noreply, State#state{radars = NewRadars}, {continue, [inc_radars, redraw_stat_bar, {redraw_radar, X}]}};

handle_event(#wx{event = #wxMouse{type=right_down, x=X, y=Y}}, State) ->
  Prev = State#state.click_info#click_info.key,
  case find_object(reclip(X, Y, wxPanel:getSize(State#state.canvas)), State#state.radars) of
    none ->
      PickedRadars = case sets:is_empty(State#state.click_info#click_info.selected) of
        true -> State#state.radars;
        false -> maps:with(sets:to_list(State#state.click_info#click_info.selected), State#state.radars)
      end,
      maps:foreach(fun(Pid, #radar_info{node = Node} = RadarInfo) ->
                       case find_angle(RadarInfo, {X, Y}) of
                         Ang when Ang >= 0 andalso Ang =< 180 ->
                           gen_server:abcast([Node], operator, {telemeter, Pid, Ang});
                         _ -> ok
                       end
                   end, PickedRadars),
      {noreply, State};
    {Prev, _} ->
      spawn(fun() ->
        gen_server:cast({global, ?SERVER}, {Prev,
                                               [{ultrasonic, 0, 150}, {ultrasonic, 45, 150}, {ultrasonic, 90, 150}]
                                              }) end),
      {noreply, State};
    {SelectionKey, Object} ->
      Env = wx:get_env(),
      spawn(fun() ->
                slider_dialog(Env, {-180, 180, Object#radar_info.angle},
                              fun(Angle) ->
                                  update_angle(SelectionKey, Angle)
                              end, "Pick Radar Angle")
            end),
      {noreply, State}
  end;

handle_event(#wx{event = #wxMouse{type=left_down, x=X, y=Y}},
             #state{radars = Radars, click_info = #click_info{selected = Selected}} = State) ->
  Update_Radar_Bitmaps =
  fun
    (undefined, RadarsMap, _Path) ->
      RadarsMap;
    (K, RadarsMap, Path) ->
      try maps:update_with(K, fun(Info) ->
                                    wxBitmap:destroy(Info#radar_info.bitmap),
                                    Bmp = get_image_bitmap(Path, Info#radar_info.angle),
                                    Info#radar_info{bitmap = Bmp}
                                end, RadarsMap) of
        NewRadars ->
          NewRadars
      catch
        _Err:{badkey, _} ->
          RadarsMap
      end
  end,
  case find_object(reclip(X, Y, wxPanel:getSize(State#state.canvas)), Radars) of
    none ->
      NewRadars = sets:fold(fun(K, OldRadars) ->
                                Update_Radar_Bitmaps(K, OldRadars, ?RADAR_DRAWING)
                            end,
                            Radars, Selected),
      {noreply, State#state{radars = NewRadars,
                            click_info = #click_info{selected = sets:new()}
                           }, {continue, redraw_radars}};
    {Key, _Object} ->
      NewRadars = Update_Radar_Bitmaps(Key, Radars, ?RADAR_DRAWING_SELECTED),
      wxPanel:connect(State#state.canvas, motion),
      wxPanel:connect(State#state.canvas, left_up),
      {X0, Y0} = _Object#radar_info.pos,
      NewOffset = {X - X0 - ?BITMAP_WIDTH, Y - Y0 - ?BITMAP_HEIGHT},
      {noreply, State#state{radars = NewRadars,
                            click_info = #click_info{key = Key, offset = NewOffset, selected = sets:add_element(Key, Selected)}
                           }, {continue, [redraw_radars]}}
  end;

handle_event(#wx{event = #wxMouse{type=motion, x=X1, y=Y1}} = _Cmd,
             #state{click_info = #click_info{key = Key, offset = {Dx, Dy}}} = State) ->
  NewPos = reclip(X1-Dx, Y1-Dy, wxPanel:getSize(State#state.canvas)),
  try maps:update_with(Key, fun(Info) -> Info#radar_info{pos = NewPos} end, State#state.radars) of
    NewRadars ->
      {noreply, State#state{radars = NewRadars}, {continue, [redraw_radars]}}
  catch
    _Err:{badkey, _} ->
      {noreply, State}
  end;

handle_event(#wx{event = #wxMouse{type=left_up}}, #state{click_info = ClickInfo} = State) ->
  wxPanel:disconnect(State#state.canvas, motion),
  wxPanel:disconnect(State#state.canvas, left_up),
  {noreply, State#state{click_info = ClickInfo#click_info{key = undefined, offset = {0, 0}}}};

%% Window Events
handle_event(#wx{event = #wxSize{}}, State) ->
  {noreply, State, {continue, [redraw_radars]}};

handle_event(#wx{event = #wxIconize{}}, State) ->
  {noreply, State, {continue, [redraw_radars]}};

handle_event(#wx{event = #wxClose{}}, State) ->
  {stop, normal, State};

%% Button Events
handle_event(#wx{id=?SUS_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{click_info = #click_info{selected = Selected}} = State) ->
  case sets:is_empty(Selected) of
    true -> operator:scan_us(all);
    false -> operator:scan_us(sets:to_list(Selected))
  end,
  {noreply, State};

handle_event(#wx{id=?SLDR_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{click_info = #click_info{selected = Selected}} = State) ->
  case sets:is_empty(Selected) of
    true -> operator:scan_ldr(all);
    false -> operator:scan_ldr(sets:to_list(Selected))
  end,
  {noreply, State};

handle_event(#wx{id=?SDUAL_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{click_info = #click_info{selected = Selected}} = State) ->
  case sets:is_empty(Selected) of
    true -> operator:scan_both(all);
    false -> operator:scan_both(sets:to_list(Selected))
  end,
  {noreply, State};

  handle_event(#wx{id=?IDLE_BUTTON, event=#wxCommand{type=command_button_clicked}},
  #state{click_info = #click_info{selected = Selected}} = State) ->
case sets:is_empty(Selected) of
true -> operator:go_idle(all);
false -> operator:go_idle(sets:to_list(Selected))
end,
{noreply, State};

handle_event(#wx{id=?FILE1_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{click_info = #click_info{selected = Selected}} = State) ->
  case sets:is_empty(Selected) of
    true -> operator:do_file(all, 0);
    false -> operator:scan_both(sets:to_list(Selected), 0)
  end,
  {noreply, State};

handle_event(#wx{id=?FILE2_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{click_info = #click_info{selected = Selected}} = State) ->
  case sets:is_empty(Selected) of
    true -> operator:do_file(all, 1);
    false -> operator:scan_both(sets:to_list(Selected), 1)
  end,
  {noreply, State};

handle_event(#wx{id=?FILE3_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{click_info = #click_info{selected = Selected}} = State) ->
  case sets:is_empty(Selected) of
    true -> operator:do_file(all, 2);
    false -> operator:scan_both(sets:to_list(Selected), 2)
  end,
  {noreply, State};

handle_event(#wx{id=?STATS_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{frame = _Frame, status_bar_stats = Stats} = State) ->
  Env = wx:get_env(),
  spawn(fun() -> stats_dialog(Env, Stats) end),
  {noreply, State};

handle_event(#wx{id=?SFILE_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{frame = _Frame, chosen_file = {Dir, FName}} = State) ->
  Env = wx:get_env(),
  spawn(fun() -> file_dialog(Env,
                                  fun parse_and_send_file/1,
                                  "Pick a file to send",
                                  Dir,
                                  FName)
        end),
  {noreply, State};

handle_event(#wx{id=?STELEM_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{frame = _Frame} = State) ->
  Env = wx:get_env(),
  spawn(fun() ->
            slider_dialog(Env, {0, 180, 90},
                          fun(Angle) ->
                              send_telemeter(Angle)
                          end, "Set Radar Angle")
        end),
  {noreply, State};


handle_event(#wx{id=?BACKGROUND_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{frame = _Frame} = State) ->
  Env = wx:get_env(),
  {ok, CurrentDir} = file:get_cwd(),

  spawn(fun() -> file_dialog(Env,
                       fun(Path) -> gen_server:call({global, ?SERVER}, {new_background, Path}) end,
                       "Pick a Background", CurrentDir ++ "/backgrounds/", ?DEFAULT_BACKGROUND_DRAWING)

end),
{noreply, State};


handle_event(#wx{} = Cmd, State) ->
  io:format("got event: ~w~n", [Cmd]),
  {noreply, State}.

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

handle_call({connect_radar, Node, Info}, _From, #state{radars = Radars} = State) ->
  #{pid := Pid, name:= Name} = Info,
  Overlay = wxOverlay:new(),
  NewRadars = case dets:lookup(State#state.radar_backup, Name) of
    [] ->
      Bmp = get_image_bitmap(?RADAR_DRAWING),
      {W, H} = wxPanel:getSize(State#state.canvas),
      Pos = reclip(W div 2, H div 2, wxPanel:getSize(State#state.canvas)),
      Radars#{Pid => #radar_info{overlay = Overlay, name = Name, node = Node, pid = Pid, pos = Pos, bitmap = Bmp}};
    [{_, #radar_info{pos = {X, Y}, angle = Angle}}] ->
      Bmp = get_image_bitmap(?RADAR_DRAWING, Angle),
      Pos = reclip(X, Y, wxPanel:getSize(State#state.canvas)),
      Radars#{Pid => #radar_info{overlay = Overlay, name = Name, node = Node, pid = Pid, pos = Pos, angle = Angle, bitmap = Bmp}}
    end,
  {reply, ok, State#state{radars = NewRadars}, {continue, [{log, "Radar ~p connected~n", [Name]}, inc_radars, redraw_stat_bar, {redraw_radar, Pid}]}};

handle_call({disconnect_radar, _Node, Info}, _From,
            #state{click_info = #click_info{selected = Selected} = ClickInfo} = State) ->
  #{pid := Pid} = Info,
  try maps:take(Pid, State#state.radars) of
    {#radar_info{name = Name, bitmap = Bitmap, overlay = Overlay} = RadarInfo, NewRadars} ->
      NewRadarInfo = RadarInfo#radar_info{bitmap = undefined, pid = undefined, overlay = undefined},
      dets:insert(State#state.radar_backup, {Name, NewRadarInfo}),
      wxBitmap:destroy(Bitmap),
      wxOverlay:destroy(Overlay),
      NewSelected = sets:del_element(Pid, Selected),
      {reply, ok, State#state{radars = NewRadars,
                              click_info = ClickInfo#click_info{selected = NewSelected}
                             }, {continue, [{log, "Radar ~p disconnected~n", [Name]}, dec_radars, redraw_stat_bar, redraw_radars]}}
  catch
    _Err:{badkey, _} ->
      {reply, ok, State}
  end;

handle_call({reconnect_operator, Node}, _From, #state{click_info = ClickInfo} = State) ->
  NewRadars = maps:filter(fun(_Key, #radar_info{name = Name, bitmap = Bitmap,
                                      overlay = Overlay, node = INode} = RadarInfo) ->
                              case INode of
                                Node ->
                                  wxBitmap:destroy(Bitmap),
                                  wxOverlay:destroy(Overlay),
                                  NewRadarInfo = RadarInfo#radar_info{bitmap = undefined,
                                                                      pid = undefined,
                                                                      overlay = undefined},
                                  dets:insert(State#state.radar_backup, {Name, NewRadarInfo}),
                                  false;
                                _ -> true
                                end
                          end, State#state.radars),
  NewSelected = sets:filter(fun(Elem) ->
                              Info = maps:get(Elem, State#state.radars, #radar_info{node = undefined}),
                              Info#radar_info.node /= Node
                            end, ClickInfo#click_info.selected),
  {reply, ok, State#state{radars = NewRadars,
                          click_info = ClickInfo#click_info{selected = NewSelected}
                         }, {continue, [{log, "Operator ~p recconected~n", [Node]}, set_radar_nums, redraw_stat_bar, redraw_radars]}};

handle_call({update_angle, Key, Angle}, _From, State) ->
  try maps:update_with(Key,
                       fun(Info) ->
                           wxBitmap:destroy(Info#radar_info.bitmap),
                           Path = case sets:is_element(Key, State#state.click_info#click_info.selected) of
                                    true -> ?RADAR_DRAWING_SELECTED;
                                    false -> ?RADAR_DRAWING
                                  end,
                           Bmp = get_image_bitmap(Path, Angle),
                           Info#radar_info{bitmap = Bmp, angle = Angle}
                       end, State#state.radars) of
    NewRadars ->
      {reply, ok, State#state{radars = NewRadars}, {continue, redraw_radars}}
  catch
    _Err:{badkey, _} ->
      {reply, ok, State}
  end;

handle_call({send_file, File, {_Path, _Name} = NewFLoc}, _From,
              #state{click_info = #click_info{selected = Selected}} = State) ->
  case sets:is_empty(Selected) of
    true -> operator:send_file(all, File);
    false -> operator:send_file(sets:to_list(Selected), File)
  end,
  {reply, ok, State#state{chosen_file = NewFLoc}};


handle_call({send_telemeter, Angle}, _From,
              #state{click_info = #click_info{selected = Selected}} = State) ->
  case sets:is_empty(Selected) of
    true -> operator:telemeter(all, Angle);
    false -> operator:telemeter(sets:to_list(Selected), Angle)
  end,
  {reply, ok, State};

handle_call({new_background, NewPath}, _From,
              #state{background = {BackgroundOverlay, BackgroundBitmap,
                                                   _BackgroundPath}} = State) ->
  {reply, ok, State#state{background = {BackgroundOverlay, BackgroundBitmap, NewPath}},
  {continue, [load_background, redraw_background, redraw_radars]}};


handle_call(_Request, _From, State) ->
  io:format("~w~n", [_Request]),
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

handle_cast({Pid, Samples}, State) when (is_pid(Pid) orelse is_integer(Pid)) andalso is_list(Samples) ->
  try maps:update_with(Pid, fun(#radar_info{samples = OldSamples} = RadarInfo) ->
                            SamplesTime = lists:map(fun({Type, Angle, Dist}) ->
                                                        {Type, erlang:monotonic_time(millisecond), Angle, Dist}
                                                    end, Samples),
                            NewSamples = SamplesTime ++ OldSamples,
                            RadarInfo#radar_info{samples = NewSamples}
                        end, State#state.radars) of
    NewRadars ->
      {noreply, State#state{radars = NewRadars}, {continue, [{draw_samples, Pid}, set_detections, redraw_detections_bar]}}
  catch
    _Err:{badkey, _} -> {noreply, State}
  end;

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term() | #wx{}, State :: term()) ->
  {noreply, NewState :: term()} |
  {noreply, NewState :: term(), Timeout :: timeout()} |
  {noreply, NewState :: term(), hibernate} |
  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info({advance_uptime}, #state{status_bar = StatusBar, status_bar_stats = #stats{uptime = Uptime} = Stats} = State) ->
  {_Days, {Hours, Minutes, Seconds}} = calendar:seconds_to_daystime(Uptime),
  UptimeStr = io_lib:format("~2..0w:~2..0w:~2..0w", [Hours, Minutes, Seconds]),
  wxStatusBar:setStatusText(StatusBar, "Uptime: " ++ UptimeStr, [{number, 0}]),
  UpdatedState = State#state{status_bar_stats = Stats#stats{uptime = Uptime + 1}},
  case Uptime rem 3 of
    0 ->
      {noreply, UpdatedState, {continue, [draw_samples, set_detections, redraw_detections_bar]}};
    _ ->
      {noreply, UpdatedState}
  end;

handle_info(#wx{} = WxEvent, State) ->
  handle_event(WxEvent, State);

handle_info({nodedown, Node}, #state{click_info = ClickInfo} = State) ->
  NewRadars = maps:filter(fun(_Key, #radar_info{name = Name, bitmap = Bitmap,
                                      overlay = Overlay, node = INode} = RadarInfo) ->
                              case INode of
                                Node ->
                                  wxBitmap:destroy(Bitmap),
                                  wxOverlay:destroy(Overlay),
                                  NewRadarInfo = RadarInfo#radar_info{bitmap = undefined,
                                                                      pid = undefined,
                                                                      overlay = undefined},
                                  dets:insert(State#state.radar_backup, {Name, NewRadarInfo}),
                                 false;
                                _ -> true
                                end
                          end, State#state.radars),
  NewSelected = sets:filter(fun(Elem) ->
                              Info = maps:get(Elem, State#state.radars, #radar_info{node = undefined}),
                              Info#radar_info.node /= Node
                            end, ClickInfo#click_info.selected),
  {noreply, State#state{radars = NewRadars,
                        click_info = ClickInfo#click_info{selected = NewSelected}},
            {continue, [set_radar_nums, dec_nodes, redraw_stat_bar, redraw_radars,
                         {log,  "node ~p disconnected~n", [Node]}]}};

handle_info({nodeup, Node}, State) ->
  {noreply, State, {continue, [inc_nodes, redraw_stat_bar, {log,  "new node connected ~p~n", [Node]}]}};

handle_info(_Info, State) ->
  io:format("got unknown info: ~p~n", [_Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all continue call, we use it to update the graphics after
%% handling transition logic
%% @end
%%--------------------------------------------------------------------
-spec handle_continue(Continue :: term(), State :: term()) ->
  {noreply, NewState :: term()} |
  {noreply, NewState :: term(), Timeout :: timeout()} |
  {noreply, NewState :: term(), hibernate} |
  {noreply, NewState :: term(), {continue, Continue :: term()}} |
  {stop, Reason :: normal | term(), NewState :: term()}.

handle_continue(Continue, State) when is_list(Continue) ->
  NewState = lists:foldl(fun do_cont/2, State, Continue),
  {noreply, NewState};

handle_continue(Continue, State) ->
  {noreply, do_cont(Continue, State)}.

do_cont(redraw_radars, State) ->
  redraw_radars(State),
  State;

do_cont({redraw_radar, Key}, State) ->
  DC = wxWindowDC:new(State#state.canvas),
  Font = wxFont:new(8, ?wxFONTFAMILY_MODERN, ?wxFONTSTYLE_NORMAL,
                    ?wxFONTWEIGHT_EXTRABOLD),
  wxDC:setFont(DC, Font),
  wxFont:destroy(Font),
  #{Key := #radar_info{overlay = Overlay, bitmap = Bmp, pos = {X, Y} = Pos, angle = Angle, node = Node}} = State#state.radars,
	DCO = wxDCOverlay:new(Overlay, DC),
  wxDCOverlay:clear(DCO),
  wxOverlay:reset(Overlay),
	wxDCOverlay:clear(DCO),
  case sets:is_element(Key, State#state.click_info#click_info.selected) of
    true ->
      PositionText = io_lib:format("~p~n(~p, ~p) ", [Node, X, Y]),
      LastText = io_lib:format("~p", [Angle]),
      FinalText = erlang:iolist_to_binary([PositionText, unicode:characters_to_binary("∡"), LastText]),
      wxDC:drawLabel(DC, FinalText,
      {X - 10, Y + 2*?BITMAP_HEIGHT, 1, 1}, [{alignment, ?wxALIGN_LEFT}]);
    false -> ok
  end,
  wxDC:drawBitmap(DC, Bmp, Pos),
  State;

do_cont({log, Str, Args}, #state{noti_box = TextCtrl} = State) ->
  case wxTextCtrl:getNumberOfLines(TextCtrl) of
    N when N > 20 ->
      Data = wxTextCtrl:getValue(State#state.noti_box),
      {ok, Dev} = file:open("radar_log.txt", [append]),
      file:write(Dev, Data),
      file:close(Dev);
    _ -> ok
  end,
  append_textbox(TextCtrl, Str, Args),
  State;

do_cont(load_background, #state{background = Background, canvas = Canvas} = State) ->
  {BackgroundOverlay, BackgroundBitmap, BackgroundPath} = Background,
  wxBitmap:destroy(BackgroundBitmap),
  {W, H} = wxPanel:getSize(Canvas),
  NewBackgroundBitmap = get_image_bitmap(BackgroundPath, 0, W, H),
  NewBackground = {BackgroundOverlay, NewBackgroundBitmap, BackgroundPath},
  State#state{background = NewBackground};

do_cont(redraw_background, #state{background = Background,canvas = Canvas} = State) ->
  redraw_background(Canvas, Background),
  State;


do_cont(redraw_stat_bar, #state{status_bar_stats = #stats{num_radars = NumRadars,
                                                          num_nodes = NumNodes} = _Stats
                               } = State) ->
  StatusText = io_lib:format("Nodes/Radars connected: ~B/~B", [NumNodes, NumRadars]),
  wxStatusBar:setStatusText(State#state.status_bar, StatusText, [{number, 1}]),
  State;

do_cont(redraw_detections_bar, #state{status_bar_stats = #stats{active_detections = ActiveDetections} = _Stats
                               } = State) ->
  DetectionsText = io_lib:format("Active Detections: ~B", [ActiveDetections]),
  wxStatusBar:setStatusText(State#state.detections_bar, DetectionsText),
  State;

do_cont(inc_nodes, #state{status_bar_stats = #stats{num_nodes = NumNodes} = Stats} = State) ->
  State#state{status_bar_stats = Stats#stats{num_nodes = NumNodes + 1}};

do_cont(dec_nodes, #state{status_bar_stats = #stats{num_nodes = NumNodes} = Stats} = State) ->
  State#state{status_bar_stats = Stats#stats{num_nodes = NumNodes - 1}};

do_cont(inc_radars, #state{status_bar_stats = #stats{num_radars = NumRadars} = Stats} = State) ->
  State#state{status_bar_stats = Stats#stats{num_radars = NumRadars + 1}};

do_cont(dec_radars, #state{status_bar_stats = #stats{num_radars = NumRadars} = Stats} = State) ->
  State#state{status_bar_stats = Stats#stats{num_radars = NumRadars - 1}};

do_cont(set_radar_nums, #state{status_bar_stats = Stats} = State) ->
  State#state{status_bar_stats = Stats#stats{num_radars = map_size(State#state.radars)}};

do_cont(set_detections, #state{status_bar_stats = Stats} = State) ->
  DetectionNum = maps:fold(fun(_Key, #radar_info{samples = Samples}, Acc) -> Acc + length(Samples) end, 0, State#state.radars),
  State#state{status_bar_stats = Stats#stats{active_detections = DetectionNum}};

do_cont(draw_samples, State) ->
  DC = wxWindowDC:new(State#state.canvas),
  TimeNow = erlang:monotonic_time(millisecond),
  NewRadars = maps:map(fun(_Key, #radar_info{overlay = Overlay, pos = Pos, samples = Samples, angle = RadarAngle} = RadarInfo) ->
                          DCO = wxDCOverlay:new(Overlay, DC),
                          wxDCOverlay:clear(DCO),
                          NewSamples = lists:filter(fun
                                                      ({_, Time, _, _}) when TimeNow - Time > 3000 -> false;
                                                      (Sample) ->
                                                        draw_sample(Sample, Pos, TimeNow, RadarAngle, DC),
                                                        true
                                                    end, Samples),
                          wxDCOverlay:destroy(DCO),
                          RadarInfo#radar_info{samples = NewSamples}
                      end, State#state.radars),
	wxWindowDC:destroy(DC),
  State#state{radars = NewRadars};

%% [{ldr, Angle, Distance}]
%% [{ultrasonic, Angle, Distance}]
do_cont({draw_samples, Pid}, #state{canvas = Canvas} = State) ->
  #{Pid := #radar_info{overlay = Overlay, pos = Pos, samples = Samples, angle = RadarAngle} = RadarInfo} = State#state.radars,
  DC = wxWindowDC:new(Canvas),
  DCO = wxDCOverlay:new(Overlay, DC),
  wxDCOverlay:clear(DCO),
  TimeNow = erlang:monotonic_time(millisecond),
  NewSamples = lists:filter(fun
                              ({_, Time, _, _}) when TimeNow - Time > 3000 -> false;
                              (Sample) ->
                                draw_sample(Sample, Pos, TimeNow, RadarAngle, DC),
                                true
                            end, Samples),
  wxDCOverlay:destroy(DCO),
	wxWindowDC:destroy(DC),
  NewRadars = (State#state.radars)#{Pid := RadarInfo#radar_info{samples = NewSamples}},
  State#state{radars = NewRadars};

do_cont(remove_samples, #state{radars = Radars} = State) ->
  NewRadars = maps:map(fun(_Pid, RadarInfo) -> RadarInfo#radar_info{samples = []} end, Radars),
  State#state{radars = NewRadars}.

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

terminate(_Reason, State) ->
  append_textbox(State#state.noti_box, "~s, radar app exiting~n", [get_current_time_str(calendar)]),
  Data = wxTextCtrl:getValue(State#state.noti_box),
  {ok, Dev} = file:open("radar_log.txt", [append]),
  file:write(Dev, Data),
  file:close(Dev),
  dets:close(State#state.radar_backup),
  maps:foreach(fun(_Key, #radar_info{bitmap = Bitmap, overlay = Overlay}) ->
                   wxOverlay:destroy(Overlay),
                   wxBitmap:destroy(Bitmap)
                end, State#state.radars),
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

-spec slider_dialog(Env :: any(),
                    Pos :: {Low :: integer(), High :: integer(), Def :: integer()},
                    Callback :: fun((integer()) -> any()),
                    Title :: string()) -> ok.

slider_dialog(Env, {Low, High, Def}, Callback, Title) ->
  wx:set_env(Env),
  SliderDialog = wxDialog:new(wx:null(), ?wxID_ANY, Title,
                              [
                               {style, ?wxDEFAULT_DIALOG_STYLE}
                              ]),
  DialogSizer = wxBoxSizer:new(?wxVERTICAL),

  Buttons = wxDialog:createButtonSizer(SliderDialog, ?wxOK bor ?wxCANCEL),
  Slider = wxSlider:new(SliderDialog, ?wxID_ANY, Def, Low, High,
                        [
                         {style, ?wxSL_HORIZONTAL bor ?wxSL_LABELS bor ?wxSL_BOTTOM}
                        ]),
  wxSizer:add(DialogSizer, Slider,
              [
               {flag, ?wxEXPAND bor ?wxALIGN_CENTER bor ?wxALL},
               {border, 5}
              ]),
  wxSizer:add(DialogSizer, Buttons,
              [
               {flag, ?wxEXPAND bor ?wxALIGN_CENTER bor ?wxALL},
               {border, 5}
              ]),

  wxDialog:setSizer(SliderDialog, DialogSizer),
  wxSizer:setSizeHints(DialogSizer, SliderDialog),

  case wxDialog:showModal(SliderDialog) of
    ?wxID_OK ->
      Angle = wxSlider:getValue(Slider),
      Callback(Angle);
    ?wxID_CANCEL ->
      ok
  end,
  wxDialog:destroy(SliderDialog),
  ok.

-spec error_dialog(Env :: any(), Title :: string(), ErrorMessage :: string()) ->
   ok.

error_dialog(Env, Title, ErrorMessage) ->
  wx:set_env(Env),
  ErrorDialog = wxMessageDialog:new(wx:null(), ErrorMessage,
                                    [{caption, Title}]
                                  ),
  wxMessageDialog:showModal(ErrorDialog),
  wxMessageDialog:destroy(ErrorDialog).


-spec file_dialog(Env :: any(),
                       Callback :: fun((string()) -> any()),
                       Title :: string(),
                       DefaultDir :: string(),
                       DefaultFile :: string()) -> ok.

file_dialog(Env, Callback, Title) ->
  file_dialog(Env, Callback, Title, "~", "").

file_dialog(Env, Callback, Title, DefaultDir, DefaultFile) ->
  wx:set_env(Env),
  FileDialog = wxFileDialog:new(wx:null(),
                                [
                                 {message, Title},
                                 {style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST bor ?wxFD_PREVIEW},
                                 {defaultDir, DefaultDir},
                                 {defaultFile, DefaultFile}
                                ]),
  case wxFileDialog:showModal(FileDialog) of
    ?wxID_OK ->
      FilePath = wxFileDialog:getPath(FileDialog),
      Callback(FilePath);
    ?wxID_CANCEL ->
      ok
  end,
  wxFileDialog:destroy(FileDialog),
  ok.

stats_dialog(Env, Stats) ->
  wx:set_env(Env),
  StatsDialog = wxDialog:new(wx:null(), ?wxID_ANY, "Stats Report", [
                                                                    {style, ?wxDEFAULT_DIALOG_STYLE bor ?wxRESIZE_BORDER}
                                                                   ]),
  StatsSizer = wxBoxSizer:new(?wxVERTICAL),
  Buttons = wxDialog:createButtonSizer(StatsDialog, ?wxOK),

  Font = wxFont:new(9, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?
                    wxFONTWEIGHT_BOLD),
  wxListCtrl:setFont(StatsDialog, Font),
  wxFont:destroy(Font),
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
          wxListCtrl:setItemBackgroundColour(ListCtrl, Idx, {240, 240, 240, 255})
      end,
      ok
  end,
  Names = record_info(fields, stats),
  [_ | Values] = tuple_to_list(Stats),
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


%%%===================================================================
%%% Auxilary Internal functions
%%%===================================================================

find_object({_X, _Y} = Pos, Objects) ->
  get_first_pred(fun(_Key, Object) -> is_in_box(Pos, Object#radar_info.pos) end, Objects).

get_first_pred(Fun, Map) when is_map(Map) ->
  pred_on_iter(Fun, maps:iterator(Map));
get_first_pred(_Fun, Map) ->
  {badmap, Map}.

pred_on_iter(Fun, Iter) ->
  case maps:next(Iter) of
    none -> none;
    {Key, Value, NextIter} ->
      case Fun(Key, Value) of
        true -> {Key, Value};
        false -> pred_on_iter(Fun, NextIter)
      end
  end.

is_in_box({X, Y} = _Actual, {X0, Y0} = _Pos) ->
  case {X - X0, Y - Y0} of
    {Xdiff, Ydiff} when Xdiff >= -?BITMAP_WIDTH andalso Xdiff =< ?BITMAP_WIDTH andalso Ydiff >= -?BITMAP_HEIGHT andalso Ydiff =< ?BITMAP_HEIGHT ->
      true;
    _ ->
      false
  end.


draw_radar_on_dc(_Key, #radar_info{pos = Pos, overlay = Overlay, bitmap = Bmp}, false, DC) ->
  DCO = wxDCOverlay:new(Overlay, DC),
  wxDCOverlay:clear(DCO),
  wxOverlay:reset(Overlay),
  wxDC:drawBitmap(DC, Bmp, Pos),
  wxDCOverlay:destroy(DCO),
  ok;

draw_radar_on_dc(_Key, #radar_info{pos = {X, Y} = Pos, angle = Angle, overlay = Overlay, bitmap = Bmp, node = Node}, true, DC) ->
  DCO = wxDCOverlay:new(Overlay, DC),
  wxDCOverlay:clear(DCO),
  wxDC:drawBitmap(DC, Bmp, Pos),
  PositionText = io_lib:format("~p~n(~p, ~p) ", [Node, X, Y]),
  LastText = io_lib:format("~p", [Angle]),
  FinalText = erlang:iolist_to_binary([PositionText, unicode:characters_to_binary("∡"), LastText]),
  wxDC:drawLabel(DC, FinalText,
        {X - 10, Y + 2*?BITMAP_HEIGHT, 1, 1}, [{alignment, ?wxALIGN_LEFT}]),
  wxOverlay:reset(Overlay),
  wxDCOverlay:destroy(DCO),
  ok.

redraw_radars(#state{canvas = Canvas, background = {
  BackgroundOverlay, BackgroundBitmap, _BackgroundPath},
  click_info = #click_info{selected = Selected}, radars = Radars} = _State) ->
  {W, H} = wxPanel:getSize(Canvas),
  Bitmap = wxBitmap:new(W, H),
  Fun = fun(DC) ->
            DCO = wxDCOverlay:new(BackgroundOverlay, DC),
            wxDCOverlay:clear(DCO),
            wxDC:drawBitmap(DC, BackgroundBitmap, {0, 0}),

            Font = wxFont:new(8, ?wxFONTFAMILY_MODERN, ?wxFONTSTYLE_NORMAL,
                              ?wxFONTWEIGHT_EXTRABOLD),
            wxDC:setFont(DC, Font),
            wxFont:destroy(Font),
            maps:foreach(fun(Key, RadarInfo) ->
                             wxOverlay:reset(RadarInfo#radar_info.overlay),
                             draw_radar_on_dc(Key, RadarInfo, sets:is_element(Key, Selected), DC)
                         end, Radars),
            wxDCOverlay:destroy(DCO)
        end,
  draw(Canvas, Bitmap, Fun),
  wxBitmap:destroy(Bitmap).

draw(Canvas, Bitmap, Fun) ->
  MemoryDC = wxMemoryDC:new(Bitmap),
  CDC = wxPaintDC:new(Canvas),

  Fun(MemoryDC),
  wxDC:blit(CDC, {0,0},
            {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
            MemoryDC, {0,0}),

  wxPaintDC:destroy(CDC),
  wxMemoryDC:destroy(MemoryDC).

redraw_background(Canvas, {BackgroundOverlay, BackgroundBitmap, _BackgroundPath} = _Background) ->
  BackgroundDC = wxWindowDC:new(Canvas),
  wxOverlay:reset(BackgroundOverlay),
  BackgroundDCO = wxDCOverlay:new(BackgroundOverlay, BackgroundDC),
  wxDC:drawBitmap(BackgroundDC, BackgroundBitmap, {0, 0}),
  %% timer:sleep(2000), % Just temporary to debug background effects
  wxDCOverlay:destroy(BackgroundDCO),
  wxWindowDC:destroy(BackgroundDC).

get_image_bitmap(Path) ->
  get_image_bitmap(Path, 0).

get_image_bitmap(Path, Angle) ->
  get_image_bitmap(Path, Angle, 2*?BITMAP_WIDTH, 2*?BITMAP_HEIGHT).

get_image_bitmap(Path, Angle, Width, Height) ->
  Rads = -Angle / 180 * math:pi(),
  Image = wxImage:new(Path),
  Image2 = wxImage:scale(Image, Width, Height, [{quality, ?wxIMAGE_QUALITY_HIGH}]),
  wxImage:setMaskColour(Image2, 255, 255, 255),
  Image3 = wxImage:rotate(Image2, Rads, {?BITMAP_WIDTH, ?BITMAP_HEIGHT}, [{interpolating, true}]),
  Bmp = wxBitmap:new(Image3),
  wxImage:destroy(Image),
  wxImage:destroy(Image2),
  wxImage:destroy(Image3),
  Bmp.

update_angle(Key, Angle) ->
  gen_server:call({global, ?SERVER}, {update_angle, Key, Angle}).

send_telemeter(Angle) ->
  gen_server:call({global, ?SERVER}, {send_telemeter, Angle}).

reclip(X, Y, {W, H}) ->
  F = fun(N, Min, Max) ->
        max(Min, min(N, Max))
      end,
  {F(X, 0, W) - ?BITMAP_WIDTH, F(Y, 0, H) - ?BITMAP_HEIGHT}.

parse_and_send_file(Path) ->
  try radar_parser:parse_file(Path) of
    ParsedFile ->
      DirPath = filename:dirname(Path),
      BaseName = filename:basename(Path),
      gen_server:call({global, ?SERVER}, {send_file, ParsedFile, {DirPath, BaseName}})
  catch
    throw:Err ->
      Env = wx:get_env(),
      Msg = io_lib:format("Err ~p~n", [Err]),
      error_dialog(Env, "Error", Msg)
  end,
  ok.

get_current_time_str(Type) ->
  {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:local_time(),
  case Type of
    full ->
      io_lib:format("~2..0B/~2..0B/~B: ~2..0B:~2..0B:~2..0B", [Day, Month, Year, Hour, Min, Sec]);
    hms ->
      io_lib:format("~2..0B:~2..0B:~2..0B", [Hour, Min, Sec]);
    calendar ->
      io_lib:format("~2..0B/~2..0B/~B", [Day, Month, Year])
  end.

append_textbox(TextCtrl, Str, Args)->
  TimeStr = get_current_time_str(hms),
  wxTextCtrl:appendText(TextCtrl, io_lib:format("~s: " ++ Str, [TimeStr | Args])),
  ok.

draw_sample({SampleType, SampleTime, Angle, Dist}, {X, Y}, TimeNow, RadarAngle, DC) ->
  {Xc, Yc} = Center = {X + ?BITMAP_WIDTH, Y + ?BITMAP_HEIGHT},        % Centered around the middle of the radar bitmap
  Rads = (RadarAngle - Angle) / 180 * math:pi(),
  {Brush, PixDist} = case SampleType of
    ultrasonic ->
      {wxBrush:new(?wxRED), Dist * ?DIST_SCALE};
    ldr ->
      {wxBrush:new(?wxBLUE), Dist * ?DIST_SCALE}
    end,
  {Xs, Ys} = Goal = {round(Xc + PixDist*math:cos(Rads)), round(Yc + PixDist*math:sin(Rads))},
  wxDC:setBrush(DC, Brush),
  wxDC:drawLine(DC, Center, Goal),
  wxDC:drawText(DC, io_lib:format("~B", [Dist]), {Xs+4, Ys+4}),
  TimeDiff = 4 - ((TimeNow - SampleTime) div 1000),
  wxDC:drawCircle(DC, Goal, 2*TimeDiff).

find_angle(#radar_info{pos = {X0, Y0}, angle = Angle}, {X1, Y1}) ->
  round(math:atan2(Y1-Y0, X1-X0) * 180 / math:pi()) + 180 - Angle.

