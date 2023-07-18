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

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2, terminate/2, code_change/3, format_status/2]).

%% internal usage
-export([]).

-define(SERVER, ?MODULE).

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
          status_bar_stats,
          click_info,
          radars
}).

-record(click_info, {
          key,
          selected
}).

-record(radar_info, {
          pos,
          angle = 0,
          bitmap,
          node,
          pid
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
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
                      {error, Error :: {already_started, pid()}} |
                      {error, Error :: term()} |
                      ignore.

start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

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
  process_flag(trap_exit, true),
  wx:new(),
  Frame = wxFrame:new(wx:null(), 1, "Radar"),
  % spawn windows
  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  StatusBar = wxStatusBar:new(Frame),
  wxStatusBar:setFieldsCount(StatusBar, 3, [{widths, [150, 200, 100]}]),
  wxStatusBar:setStatusText(StatusBar, "Uptime: 00:00:00", [{number, 0}]),
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

  wxPanel:connect(Canvas, paint, [callback]),
  wxPanel:connect(Canvas, size),
  wxPanel:connect(Canvas, left_down),
  wxPanel:connect(Canvas, right_down),
  wxPanel:connect(Canvas, middle_down),
  {ok, _TRef} = timer:send_interval(1000, {advance_uptime}),
  wxFrame:show(Frame),
  {ok, #state{frame = Frame, canvas = Canvas,
              status_bar = StatusBar, status_bar_stats = #stats{},
              click_info = #click_info{selected = sets:new()}, radars = #{}}}.

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

handle_event(#wx{event = #wxMouse{type=middle_down, x=X, y=Y}},
             #state{radars = Radars} = State) ->
  Bmp = get_image_bitmap("imgs/radar-drawing.jpeg"),
  NewRadars = Radars#{X => #radar_info{pos = {X-20, Y-20}, bitmap = Bmp}},
  {noreply, State#state{radars = NewRadars}, {continue, [redraw_radars]}};

handle_event(#wx{event = #wxMouse{type=right_down, x=X, y=Y}}, State) ->
  case find_object({X-20, Y-20}, State#state.radars) of
    none ->
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
  case find_object({X-20, Y-20}, Radars) of
    none ->
      NewRadars = sets:fold(fun(K, OldRadars) ->
                                Update_Radar_Bitmaps(K, OldRadars, "imgs/radar-drawing.jpeg")
                            end,
                            Radars, Selected),
      {noreply, State#state{radars = NewRadars,
                            click_info = #click_info{selected = sets:new()}
                           }, {continue, [redraw_radars]}};
    {Key, _Object} ->
      NewRadars = Update_Radar_Bitmaps(Key, Radars, "imgs/radar-drawing-selected.jpeg"),
      wxPanel:connect(State#state.canvas, motion),
      wxPanel:connect(State#state.canvas, left_up),
      {noreply, State#state{radars = NewRadars,
                            click_info = #click_info{key = Key, selected = sets:add_element(Key, Selected)}
                           }, {continue, [redraw_radars]}}
  end;

handle_event(#wx{event = #wxMouse{type=motion, x=X1, y=Y1}} = _Cmd,
             #state{click_info = #click_info{key = Key}} = State) ->
  {W, H} = wxPanel:getSize(State#state.canvas),
  NewPos = reclip(X1, Y1, {W, H}),
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
  {noreply, State#state{click_info = ClickInfo#click_info{key = undefined}}};

handle_event(#wx{event = #wxSize{}}, State) ->
  {noreply, State, {continue, [redraw_radars]}};

handle_event(#wx{event = #wxClose{}}, State) ->
  {stop, normal, State};

handle_event(#wx{id=?SUS_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{} = State) ->
  % do something with the button
  io:format("Scan button pressed~n"),
  {noreply, State};

handle_event(#wx{id=?SLDR_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{} = State) ->
  {noreply, State};

handle_event(#wx{id=?SDUAL_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{} = State) ->
  {noreply, State};

handle_event(#wx{id=?FILE1_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{} = State) ->
  {noreply, State};

handle_event(#wx{id=?FILE2_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{} = State) ->
  {noreply, State};

handle_event(#wx{id=?FILE3_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{} = State) ->

  {noreply, State};

handle_event(#wx{id=?STATS_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{frame = _Frame, status_bar_stats = Stats} = State) ->
  Env = wx:get_env(),
  spawn(fun() -> stats_dialog(Env, Stats) end),
  {noreply, State};

handle_event(#wx{id=?SFILE_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{frame = _Frame} = State) ->
  Env = wx:get_env(),
  spawn(fun() -> send_file_dialog(Env,
                                  fun(Path) ->
                                      io:format("user selected ~s~n", [Path])
                                  end, "Pick a file to send")
        end),
  {noreply, State};

handle_event(#wx{id=?STELEM_BUTTON, event=#wxCommand{type=command_button_clicked}},
             #state{frame = _Frame} = State) ->
  Env = wx:get_env(),
  spawn(fun() ->
            slider_dialog(Env, {0, 180, 90},
                          fun(Angle) ->
                              io:format("user clicked ~p~n", [Angle])
                          end, "Set Radar Angle")
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


handle_call({update_angle, Key, Angle}, _From, State) ->
  try maps:update_with(Key,
                       fun(Info) ->
                           wxBitmap:destroy(Info#radar_info.bitmap),
                           Path = case sets:is_element(Key, State#state.click_info#click_info.selected) of
                                    true -> "imgs/radar-drawing-selected.jpeg";
                                    false -> "imgs/radar-drawing.jpeg"
                                  end,
                           Bmp = get_image_bitmap(Path, Angle),
                           Info#radar_info{bitmap = Bmp, angle = Angle}
                       end, State#state.radars) of
    NewRadars ->
      {reply, ok, State#state{radars = NewRadars}, {continue, [redraw_radars]}}
  catch
    _Err:{badkey, _} ->
      {reply, ok, State}
  end;

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

handle_info({advance_uptime}, #state{status_bar = StatusBar, status_bar_stats = #stats{uptime = Uptime}} = State) ->
  {_Days, {Hours, Minutes, Seconds}} = calendar:seconds_to_daystime(Uptime),
  UptimeStr = io_lib:format("~2..0w:~2..0w:~2..0w", [Hours, Minutes, Seconds]),
  wxStatusBar:setStatusText(StatusBar, "Uptime: " ++ UptimeStr, [{number, 0}]),
  UpdatedState = State#state{status_bar_stats = #stats{ uptime = Uptime + 1}},
  {noreply, UpdatedState};

handle_info(#wx{} = WxEvent, State) ->
  handle_event(WxEvent, State);

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
  case lists:member(redraw_radars, Continue) of
    true ->
      redraw_radars(State#state.canvas, State#state.radars);
    false ->
      ok
  end,
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

terminate(_Reason, State) ->
  radar_app:stop(State),
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
      % TODO add callback
      Callback(Angle);
    ?wxID_CANCEL ->
      ok
  end,
  wxDialog:destroy(SliderDialog),
  ok.

-spec send_file_dialog(Env :: any(),
                       Callback :: fun((string()) -> any()),
                       Title :: string()) -> ok.

send_file_dialog(Env, Callback, Title) ->
  wx:set_env(Env),
  FileDialog = wxFileDialog:new(wx:null(),
                                [
                                 {message, Title},
                                 {style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST bor ?wxFD_PREVIEW},
                                 {defaultDir, "~"},
                                 {defaultFile, ""}
                                ]),
  case wxFileDialog:showModal(FileDialog) of
    ?wxID_OK ->
      % TODO implement functionality
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
    {Xdiff, Ydiff} when Xdiff >= -20 andalso Xdiff =< 20 andalso Ydiff >= -20 andalso Ydiff =< 20 ->
      true;
    _ ->
      false
  end.

redraw_radars(Canvas, Radars) ->
  {W,H} = wxPanel:getSize(Canvas),
  Bitmap = wxBitmap:new(erlang:max(W,30),erlang:max(30,H)),
  Fun = fun(DC) ->
            wxDC:clear(DC),
            maps:foreach(fun(_, #radar_info{pos = Pos, bitmap = Bmp}) ->
                             wxDC:drawBitmap(DC, Bmp, Pos)
                         end, Radars)
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


get_image_bitmap(Path) ->
  get_image_bitmap(Path, 0).

get_image_bitmap(Path, Angle) ->
  Rads = -Angle / 180 * math:pi(),
  Image = wxImage:new(Path),
  Image2 = wxImage:scale(Image, 40, 40, [{quality, ?wxIMAGE_QUALITY_HIGH}]),
  wxImage:setMaskColour(Image2, 255, 255, 255),
  Image3 = wxImage:rotate(Image2, Rads, {20,20}, [{interpolating, true}]),
  Bmp = wxBitmap:new(Image3),
  wxImage:destroy(Image),
  wxImage:destroy(Image2),
  wxImage:destroy(Image3),
  Bmp.

update_angle(Key, Angle) ->
  gen_server:call({global, ?SERVER}, {update_angle, Key, Angle}).

reclip(X, Y, {W, H}) ->
  F = fun(N, Min, Max) ->
        max(Min, min(N, Max))
      end,
  {F(X, 0, W) - 25, F(Y, 0, H) - 20}.

