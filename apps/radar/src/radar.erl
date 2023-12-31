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

-record(state, {
          frame,
          canvas,
          background,
          detections_bar,
          status_bar,
          status_bar_stats,
          click_info,
          radars,
          radar_backup,
          noti_box,
          chosen_file = {"~", ""},
          us_max_min = {2, 450},
          draw_samples = true
}).

-record(click_info, {
          key,
          offset = {0, 0},
          selected
}).

-record(radar_info, {
          pos,
          angle = 0,
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
-define(US_DISTANCE, 111).

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

  File1Button = wxButton:new(Frame, ?FILE1_BUTTON, [{label, "Start File 1"}]),
  wxGridSizer:add(ButtonGridSizer, File1Button,
                  [{proportion, 0}, {flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_LEFT bor ?wxEXPAND}]),

  File2Button = wxButton:new(Frame, ?FILE2_BUTTON, [{label, "Start File 2"}]),
  wxGridSizer:add(ButtonGridSizer, File2Button,
                  [{proportion, 0}, {flag, ?wxALIGN_CENTER bor ?wxEXPAND}]),

  File3Button = wxButton:new(Frame, ?FILE3_BUTTON, [{label, "Start File 3"}]),
  wxGridSizer:add(ButtonGridSizer, File3Button,
                  [{proportion, 0}, {flag, ?wxALIGN_CENTER_VERTICAL bor ?wxALIGN_RIGHT bor ?wxEXPAND}]),

  PickMinMaxButton = wxButton:new(Frame, ?US_DISTANCE, [{label, "US Dist"}]),
  wxGridSizer:add(ButtonGridSizer, PickMinMaxButton,
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
  {ok, _TRef} = timer:send_interval(1000, {advance_uptime}),
  Icon = wxIcon:new("imgs/app_icon.png"),
  wxFrame:setIcon(Frame, Icon),
  wxIcon:destroy(Icon),
  wxFrame:show(Frame),
  {W, H} = wxPanel:getSize(Canvas),
  BackgroundBitmap = get_image_bitmap("backgrounds/" ++ ?DEFAULT_BACKGROUND_DRAWING, 0, W, H),
  Background = {BackgroundBitmap, "backgrounds/" ++ ?DEFAULT_BACKGROUND_DRAWING},

  StatsETS = ets:new(stats_ets, [set]),
  StatsKeys = [uptime, rec_msg, num_nodes, num_radars, active_detections],
  StatsList = [{Key, 0} || Key <- StatsKeys],
  ets:insert(StatsETS, StatsList),
  {ok, RadarBackup} = dets:open_file(radar_backup, [{auto_save, 60000}]),
  {ok, _RedrawTRef} = timer:send_after(20, redraw),
  {ok, #state{frame = Frame, canvas = Canvas, radar_backup = RadarBackup, detections_bar = DetectionsText,
              background = Background, noti_box = NotificationsBox, status_bar = StatusBar,
              status_bar_stats = StatsETS, click_info = #click_info{selected = sets:new()}, radars = #{}},
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
handle_event(#wx{event = #wxMouse{type=right_down, x=X, y=Y}}, State) ->
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
                           }};
    {Key, _Object} ->
      NewRadars = Update_Radar_Bitmaps(Key, Radars, ?RADAR_DRAWING_SELECTED),
      wxPanel:connect(State#state.canvas, motion),
      wxPanel:connect(State#state.canvas, left_up),
      {X0, Y0} = _Object#radar_info.pos,
      NewOffset = {X - X0 - ?BITMAP_WIDTH, Y - Y0 - ?BITMAP_HEIGHT},
      {noreply, State#state{radars = NewRadars,
                            click_info = #click_info{key = Key, offset = NewOffset, selected = sets:add_element(Key, Selected)}
                           }}
  end;

handle_event(#wx{event = #wxMouse{type=motion, x=X1, y=Y1}} = _Cmd,
             #state{click_info = #click_info{key = Key, offset = {Dx, Dy}}} = State) ->
  NewPos = reclip(X1-Dx, Y1-Dy, wxPanel:getSize(State#state.canvas)),
  try maps:update_with(Key, fun(Info) -> Info#radar_info{pos = NewPos} end, State#state.radars) of
    NewRadars -> {noreply, State#state{radars = NewRadars}}
  catch
    _Err:{badkey, _} -> {noreply, State}
  end;

handle_event(#wx{event = #wxMouse{type=left_up}}, #state{click_info = ClickInfo} = State) ->
  wxPanel:disconnect(State#state.canvas, motion),
  wxPanel:disconnect(State#state.canvas, left_up),
  {noreply, State#state{click_info = ClickInfo#click_info{key = undefined, offset = {0, 0}}}};

%% Window Events
handle_event(#wx{event = #wxSize{}}, State) ->
  {noreply, State, {continue, clear_samples}};

handle_event(#wx{event = #wxIconize{}}, State) ->
  {noreply, State, {continue, clear_samples}};

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
    false -> operator:do_file(sets:to_list(Selected), 0)
  end,
  {noreply, State};

handle_event(#wx{id=?FILE2_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{click_info = #click_info{selected = Selected}} = State) ->
  case sets:is_empty(Selected) of
    true -> operator:do_file(all, 1);
    false -> operator:do_file(sets:to_list(Selected), 1)
  end,
  {noreply, State};

handle_event(#wx{id=?FILE3_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{click_info = #click_info{selected = Selected}} = State) ->
  case sets:is_empty(Selected) of
    true -> operator:do_file(all, 2);
    false -> operator:do_file(sets:to_list(Selected), 2)
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

% this was fun to do
% use callbacks to open more dialogs
handle_event(#wx{id=?US_DISTANCE, event=#wxCommand{type=command_button_clicked}},
             #state{frame = _Frame, us_max_min = {Min, Max}} = State) ->
  Env = wx:get_env(),
  spawn(fun() ->
            slider_dialog(Env, {2, 450, Min},
                          fun(MinVal) ->
                              slider_dialog(Env, {2, 450, Max},
                                            fun(MaxVal) ->
                                                case MinVal > MaxVal of
                                                  true -> error_dialog(Env, "Error", "Minimum value is bigger than maximum");
                                                  false -> gen_server:call({global, ?SERVER}, {change_us_angles, MinVal, MaxVal})
                                                end
                                            end,
                                            "Pick max distance")
                          end, "Pick min distance")
        end),
  {noreply, State};

handle_event(#wx{id=?BACKGROUND_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{frame = _Frame} = State) ->
  Env = wx:get_env(),
  {ok, CurrentDir} = file:get_cwd(),

  spawn(fun() ->
            file_dialog(Env,
                        fun(Path) -> gen_server:call({global, ?SERVER}, {new_background, Path}) end,
                        "Pick a Background",
                        CurrentDir ++ "/backgrounds/",
                        ?DEFAULT_BACKGROUND_DRAWING)
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
  NewRadars = case dets:lookup(State#state.radar_backup, Name) of
    [] ->
      Bmp = get_image_bitmap(?RADAR_DRAWING),
      {W, H} = wxPanel:getSize(State#state.canvas),
      Pos = reclip(W div 2, H div 2, wxPanel:getSize(State#state.canvas)),
      Radars#{Pid => #radar_info{name = Name, node = Node, pid = Pid, pos = Pos, bitmap = Bmp}};
    [{_, #radar_info{pos = {X, Y}, angle = Angle}}] ->
      Bmp = get_image_bitmap(?RADAR_DRAWING, Angle),
      Pos = reclip(X + ?BITMAP_WIDTH, Y + ?BITMAP_HEIGHT, wxPanel:getSize(State#state.canvas)),
      Radars#{Pid => #radar_info{name = Name, node = Node, pid = Pid, pos = Pos, angle = Angle, bitmap = Bmp}}
    end,
  {reply, ok, State#state{radars = NewRadars}, {continue, [{log, "Radar ~p connected~n", [Name]}, inc_radars, inc_msg, redraw_stat_bar]}};

handle_call({disconnect_radar, _Node, Info}, _From,
            #state{click_info = #click_info{selected = Selected} = ClickInfo} = State) ->
  #{pid := Pid} = Info,
  try maps:take(Pid, State#state.radars) of
    {#radar_info{name = Name, bitmap = Bitmap} = RadarInfo, NewRadars} ->
      NewRadarInfo = RadarInfo#radar_info{bitmap = undefined, pid = undefined},
      dets:insert(State#state.radar_backup, {Name, NewRadarInfo}),
      wxBitmap:destroy(Bitmap),
      NewSelected = sets:del_element(Pid, Selected),
      {reply, ok, State#state{radars = NewRadars,
                              click_info = ClickInfo#click_info{selected = NewSelected}
                             }, {continue, [{log, "Radar ~p disconnected~n", [Name]}, dec_radars, inc_msg, redraw_stat_bar]}}
  catch
    _Err:{badkey, _} ->
      {reply, ok, State, {continue, inc_msg}}
  end;

% delete all radars belonging to the operator,
% operator himself is responsible to connect each radar later.
handle_call({reconnect_operator, Node}, _From, #state{click_info = ClickInfo} = State) ->
  NewRadars = maps:filter(fun(_Key, #radar_info{name = Name, bitmap = Bitmap, node = INode} = RadarInfo) ->
                              case INode of
                                Node ->
                                  wxBitmap:destroy(Bitmap),
                                  NewRadarInfo = RadarInfo#radar_info{bitmap = undefined,
                                                                      pid = undefined},
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
                         }, {continue, [{log, "Operator ~p reconnected~n", [Node]}, set_radar_nums, inc_msg, redraw_stat_bar]}};

% callbacks from dialogs go to handle_call
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
      {reply, ok, State#state{radars = NewRadars}, {continue, inc_msg}}
  catch
    _Err:{badkey, _} ->
      {reply, ok, State, {continue, inc_msg}}
  end;

handle_call({change_us_angles, MinVal, MaxVal}, _From, State) ->
  {reply, ok, State#state{us_max_min = {MinVal, MaxVal}}};

handle_call({send_file, File, {_Path, _Name} = NewFLoc}, _From,
              #state{click_info = #click_info{selected = Selected}} = State) ->
  case sets:is_empty(Selected) of
    true -> operator:send_file(all, File);
    false -> operator:send_file(sets:to_list(Selected), File)
  end,
  {reply, ok, State#state{chosen_file = NewFLoc}, {continue, inc_msg}};


handle_call({send_telemeter, Angle}, _From,
              #state{click_info = #click_info{selected = Selected}} = State) ->
  case sets:is_empty(Selected) of
    true -> operator:telemeter(all, Angle);
    false -> operator:telemeter(sets:to_list(Selected), Angle)
  end,
  {reply, ok, State, {continue, inc_msg}};

handle_call({new_background, NewPath}, _From,
              #state{background = {BackgroundBitmap, _BackgroundPath}} = State) ->
  case filename:dirname(NewPath) == filename:absname("shapes") of
    true ->
      {reply, ok, State, {continue, {easter_egg, NewPath}}};
    false ->
      {reply, ok, State#state{background = {BackgroundBitmap, NewPath}},
       {continue, load_background}}
  end;

handle_call(_Request, _From, State) ->
  io:format("~w~n", [_Request]),
  Reply = ok,
  {reply, Reply, State, {continue, inc_msg}}.

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

% concepually here we can easily add a function that proccesses the samples and does something
% interesting with them (for example tracking).
% we would call it on NewSamples and it would return something for the gui to draw.
handle_cast({Pid, Samples}, #state{us_max_min = {Min, Max}} = State) when is_pid(Pid) andalso is_list(Samples) ->
  try maps:update_with(Pid, fun(#radar_info{samples = OldSamples} = RadarInfo) ->
                            SamplesTime = lists:filtermap(
                                            fun
                                              ({ultrasonic, _Angle, Dist}) when Dist < Min orelse Dist > Max -> false;
                                              ({ultrasonic, _Time, _Angle, Dist}) when Dist < Min orelse Dist > Max -> false;
                                              ({Type, Angle, Dist}) ->
                                                {true, {Type, erlang:monotonic_time(millisecond), Angle, Dist}};
                                              ({Type, Time, Angle, Dist}) ->
                                                {true, {Type, erlang:monotonic_time(millisecond) + Time, Angle, Dist}}
                                            end, Samples),
                            NewSamples = SamplesTime ++ OldSamples,
                            RadarInfo#radar_info{samples = NewSamples}
                        end, State#state.radars) of
    NewRadars ->
      {noreply, State#state{radars = NewRadars}, {continue, [inc_msg, set_detections, redraw_detections_bar]}}
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

% generated from timer:send_after 20 ms after finishing drawing frame
handle_info(redraw, State) ->
  {noreply, State, {continue, redraw}};

% generated from timer:send_interval each second to update the status bar timer
handle_info({advance_uptime}, #state{status_bar = StatusBar, status_bar_stats = Stats} = State) ->
  Uptime = ets:update_counter(Stats, uptime, 1),
  {_Days, {Hours, Minutes, Seconds}} = calendar:seconds_to_daystime(Uptime),
  UptimeStr = io_lib:format("~2..0w:~2..0w:~2..0w", [Hours, Minutes, Seconds]),
  wxStatusBar:setStatusText(StatusBar, "Uptime: " ++ UptimeStr, [{number, 0}]),
  {noreply, State, {continue, [set_detections, redraw_detections_bar]}};

% redirect all wx events to handle_event
handle_info(#wx{} = WxEvent, State) ->
  handle_event(WxEvent, State);

% some operator fell, remove all it's radars, decrement the number of connected nodes
handle_info({nodedown, Node}, #state{click_info = ClickInfo} = State) ->
  NewRadars = maps:filter(fun(_Key, #radar_info{name = Name, bitmap = Bitmap, node = INode} = RadarInfo) ->
                              case INode of
                                Node ->
                                  wxBitmap:destroy(Bitmap),
                                  NewRadarInfo = RadarInfo#radar_info{bitmap = undefined,
                                                                      pid = undefined},
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
            {continue, [inc_msg, set_radar_nums, dec_nodes, redraw_stat_bar,
                         {log,  "node ~p disconnected~n", [Node]}]}};

% some operator connected, inc the number of connected nodes
handle_info({nodeup, Node}, State) ->
  {noreply, State, {continue, [inc_nodes, redraw_stat_bar, {log, "new node connected ~p~n", [Node]}]}};

handle_info(_Info, State) ->
  io:format("got unknown info: ~p~n", [_Info]),
  {noreply, State, {continue, inc_msg}}.

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

% do_cont should mutate the state and return it
% can also do side effects such as updating ets, redrawing on canvas
% anything we can imagine
handle_continue(Continue, State) when is_list(Continue) ->
  NewState = lists:foldl(fun do_cont/2, State, Continue),
  {noreply, NewState};

handle_continue(Continue, State) ->
  {noreply, do_cont(Continue, State)}.

%% [{ldr, Angle, Distance}]
%% [{ultrasonic, Angle, Distance}]
% non graphical mode, only filter the samples
do_cont(redraw, #state{draw_samples = false} = State) ->
  TimeNow = erlang:monotonic_time(millisecond),
  NewRadars = maps:map(fun(_Key, #radar_info{samples = Samples} = RadarInfo) ->
               NewSamples = lists:filter(fun
                              ({_, Time, _, _}) when TimeNow - Time > 3000 -> false;
                              (_) -> true
                            end, Samples),
               RadarInfo#radar_info{samples = NewSamples}
           end, State#state.radars),
  {ok, _RedrawTRef} = timer:send_after(20, redraw),
  State#state{radars = NewRadars};

% main graphical function, draws everything
do_cont(redraw, #state{background = {BackgroundBitmap, _},
                             click_info = #click_info{selected = Selected}} = State) ->
  {W, H} = wxPanel:getSize(State#state.canvas),
  Bitmap = wxBitmap:new(W, H),
  Fun = fun(DC) ->
            wxDC:clear(DC),
            wxDC:drawBitmap(DC, BackgroundBitmap, {0, 0}),

            Font = wxFont:new(8, ?wxFONTFAMILY_MODERN, ?wxFONTSTYLE_NORMAL,
                              ?wxFONTWEIGHT_EXTRABOLD),
            wxDC:setFont(DC, Font),
            wxFont:destroy(Font),

            TimeNow = erlang:monotonic_time(millisecond),
            NewRadars = maps:map(fun(Key, RadarInfo) -> draw_radar(Key, RadarInfo, TimeNow, Selected, DC) end, State#state.radars),
            NewRadars
        end,
  NewRadars = draw(State#state.canvas, Bitmap, Fun),
  wxBitmap:destroy(Bitmap),
  {ok, _RedrawTRef} = timer:send_after(20, redraw),
  State#state{radars = NewRadars};

do_cont(clear_samples, #state{radars = Radars} = State) ->
  NewRadars = maps:map(fun(_Pid, RadarInfo) -> RadarInfo#radar_info{samples = []} end, Radars),
  State#state{radars = NewRadars};

do_cont(inc_msg, State) ->
  ets:update_counter(State#state.status_bar_stats, rec_msg, 1),
  State;

do_cont({log, Str, Args}, #state{noti_box = TextCtrl} = State) ->
  append_textbox(TextCtrl, Str, Args),
  State;

do_cont(load_background, #state{background = {BackgroundBitmap, BackgroundPath},
                                canvas = Canvas} = State) ->
  wxBitmap:destroy(BackgroundBitmap),
  {W, H} = wxPanel:getSize(Canvas),
  NewBackgroundBitmap = get_image_bitmap(BackgroundPath, 0, W, H),
  NewBackground = {NewBackgroundBitmap, BackgroundPath},
  State#state{background = NewBackground};

do_cont(redraw_background, #state{background = {BackgroundBitmap, _}} = State) ->
  {W, H} = wxPanel:getSize(State#state.canvas),
  Bitmap = wxBitmap:new(W, H),
  Fun = fun(DC) ->
            wxDC:clear(DC),
            wxDC:drawBitmap(DC, BackgroundBitmap, {0, 0})
        end,
  draw(State#state.canvas, Bitmap, Fun),
  wxBitmap:destroy(Bitmap),
  State;

% open shapes folder from the change background button ;)
do_cont({easter_egg, Path}, State) ->
  Args = [{access, read}, {file, Path}, {type, set}, {repair, false}],
  case dets:open_file(shape, Args) of
    {ok, Ref} ->
      NewState = update_state_from_dets(Ref, State),
      dets:close(Ref),
      NewState;
    {error, _} -> State
  end;

do_cont(redraw_stat_bar, State) ->
  [{_, NumNodes}] = ets:lookup(State#state.status_bar_stats, num_nodes),
  [{_, NumRadars}] = ets:lookup(State#state.status_bar_stats, num_radars),
  StatusText = io_lib:format("Nodes/Radars connected: ~B/~B", [NumNodes, NumRadars]),
  wxStatusBar:setStatusText(State#state.status_bar, StatusText, [{number, 1}]),
  State;

do_cont(redraw_detections_bar, State) ->
  [{_, ActiveDetections}] = ets:lookup(State#state.status_bar_stats, active_detections),
  DetectionsText = io_lib:format("Active Detections: ~B", [ActiveDetections]),
  wxStatusBar:setStatusText(State#state.detections_bar, DetectionsText),
  State;

do_cont(inc_nodes, State) ->
  ets:update_counter(State#state.status_bar_stats, num_nodes, 1),
  State;

do_cont(dec_nodes, State) ->
  ets:update_counter(State#state.status_bar_stats, num_nodes, -1),
  State;

do_cont(inc_radars, State) ->
  ets:update_counter(State#state.status_bar_stats, num_radars, 1),
  State;

do_cont(dec_radars, State) ->
  ets:update_counter(State#state.status_bar_stats, num_radars, -1),
  State;

do_cont(set_radar_nums, State) ->
  ets:insert(State#state.status_bar_stats, {num_radars, map_size(State#state.radars)}),
  State;

do_cont(set_detections, State) ->
  DetectionNum = maps:fold(fun(_Key, #radar_info{samples = Samples}, Acc) -> Acc + length(Samples) end, 0, State#state.radars),
  ets:insert(State#state.status_bar_stats, {active_detections, DetectionNum}),
  State.


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

% close dets correctly, write from log box to output file.
terminate(_Reason, State) ->
  append_textbox(State#state.noti_box, "~s, radar app exiting~n", [get_current_time_str(calendar)]),
  Data = wxTextCtrl:getValue(State#state.noti_box),
  {ok, Dev} = file:open("radar_log.txt", [append]),
  file:write(Dev, Data),
  file:close(Dev),
  maps:foreach(fun(_Key, #radar_info{name = Name, bitmap = Bitmap} = RadarInfo) ->
                   wxBitmap:destroy(Bitmap),
                    NewRadarInfo = RadarInfo#radar_info{bitmap = undefined,
                                                        pid = undefined},
                    dets:insert(State#state.radar_backup, {Name, NewRadarInfo})
                end, State#state.radars),
  dets:close(State#state.radar_backup),
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

%% all dialogs are made by first getting the wx enviroment, spawning a new process
%% that calls the dialog functions.
%% else we will block the gen_server from proccessing events until the user exits
%% the dialog, additionaly this blocks interacting with the background window,
%% so we can have at most 1 dialog.

-spec slider_dialog(Env :: any(),
                    Pos :: {Low :: integer(), High :: integer(), Def :: integer()},
                    Callback :: fun((integer()) -> any()),
                    Title :: string()) -> ok.

% slider dialog, can set low, high and default value
% Callback gets called on the user selected number
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

% shows an error dialog with the error msg
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

% shows a file picker dialog, calls Callback with the selected directory
% we can also supply the default dir and file
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

% shows the stats
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
  StatsList = ets:tab2list(Stats),
  {Names, Values} = lists:unzip(StatsList),
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

% get first object
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

% regular (non-selected radar)
draw_radar_on_dc(_Key, #radar_info{pos = Pos, bitmap = Bmp}, false, DC) ->
  wxDC:drawBitmap(DC, Bmp, Pos),
  ok;

% selected radar, draw some text with it's info
draw_radar_on_dc(_Key, #radar_info{pos = {X, Y} = Pos, angle = Angle,
                                   bitmap = Bmp, node = Node, name = Name}, true, DC) ->
  wxDC:drawBitmap(DC, Bmp, Pos),
  Text = io_lib:format("~p~n~.30. s~n(~p, ~p) ∡~p", [Node, Name, X, Y, Angle]),
  wxDC:drawLabel(DC, Text,
        {X - 10, Y + 2*?BITMAP_HEIGHT, 1, 1}, [{alignment, ?wxALIGN_LEFT}]),
  ok.

draw_sample({SampleType, SampleTime, Angle, Dist}, {X, Y}, TimeNow, RadarAngle, DC) ->
  % Centered around the middle of the radar bitmap
  {Xc, Yc} = Center = {X + ?BITMAP_WIDTH, Y + ?BITMAP_HEIGHT},
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

% draws radar and its samples, additionaly filters samples older than 3000 ms.
draw_radar(_Key, #radar_info{pos = Pos, samples = Samples, angle = RadarAngle} = RadarInfo, TimeNow, Selected, DC) ->
  draw_radar_on_dc(_Key, RadarInfo, sets:is_element(_Key, Selected), DC),
  NewSamples = lists:filter(fun
                              ({_, Time, _, _}) when TimeNow - Time > 3000 -> false;
                              (Sample) -> draw_sample(Sample, Pos, TimeNow, RadarAngle, DC), true
                            end, Samples),
  RadarInfo#radar_info{samples = NewSamples}.

% use memorydc as a buffer, then copy it onto the canvas
% this protects from flickering
draw(Canvas, Bitmap, Fun) ->
  MemoryDC = wxMemoryDC:new(Bitmap),
  CDC = wxPaintDC:new(Canvas),

  Ret = Fun(MemoryDC),
  wxDC:blit(CDC, {0,0},
            {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
            MemoryDC, {0,0}),

  wxPaintDC:destroy(CDC),
  wxMemoryDC:destroy(MemoryDC),
  Ret.

% get bitmaps for different objects, such as regular/selected radar or background
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

% clip radar back into canvas
reclip(X, Y, {W, H}) ->
  F = fun(N, Min, Max) ->
        max(Min, min(N, Max))
      end,
  {F(X, 0, W) - ?BITMAP_WIDTH, F(Y, 0, H) - ?BITMAP_HEIGHT}.

% try to parse and send the file
% if it fails for any reason throw an error dialog
% also send path and name for updating the state (default dir and file)
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

% returns 0-180 if infront of the radar
% 180-360 if behind
find_angle(#radar_info{pos = {X0, Y0}, angle = Angle}, {X1, Y1}) ->
  round((math:atan2(Y1 - Y0 - ?BITMAP_HEIGHT, X1 - X0 - ?BITMAP_WIDTH) * 180 / math:pi()) + 180 - Angle) rem 360.

% easter egg
update_state_from_dets(Dets, State) ->
  NewRadars = maps:map(fun
                         (_, #radar_info{name = Name} = RadarInfo) ->
                           case dets:lookup(Dets, Name) of
                             [] -> RadarInfo;
                             [{_, #radar_info{pos = {X, Y}, angle = Angle}}] ->
                               Pos = reclip(X + ?BITMAP_WIDTH, Y + ?BITMAP_HEIGHT, wxPanel:getSize(State#state.canvas)),
                               RadarInfo#radar_info{pos = Pos, angle = Angle}
                           end
                       end, State#state.radars),
  State#state{radars = NewRadars}.

