%%%-------------------------------------------------------------------
%%% @author jonathankarin
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jul 2020 12:23
%%%-------------------------------------------------------------------
-module(gui).
-author("jonathankarin").

-behaviour(wx_object).


-import(util,[integer_to_atom/1]).
-export([start/1, init/1, terminate/2, code_change/3,
  handle_info/2, handle_call/3, handle_cast/2, handle_event/2,  start_global/1]).

-include_lib("wx/include/wx.hrl").

-define(SERVER,?MODULE).


-record(state,
{
clicked,
  algchooser,
  sourcechooser,
  destchooser,
  file,
  filechooser,
  node1,
  node2,
  node3,
  node4,
  log,
  frame,
  master,
  panel}).


start_global(Node) ->
  wx_object:start_link({local,?SERVER},?MODULE,[global,Node],[]).

start(Node) ->
  wx_object:start_link({local,?SERVER},?MODULE,[local,Node],[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Mode,Node]) ->
  InitState = init_gui(Mode,Node),
  {InitState#state.frame,InitState}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%handle file choosing
%active_alg(SMList, LocalFile,Root, Dest, SP,MST,SPT,Log);

%.



handle_event(#wx{obj = FilePicker, event = #wxCommand{type = command_filepicker_changed}},
    State = #state{frame = Frame , log = Log}) ->
  File = wxFilePickerCtrl:getPath(FilePicker),
  FileL = length(lists:flatten(File)),
  if
    FileL > 1 ->
      Tokens=string:tokens(File,"/"),
      LocalFile=lists:last(Tokens),
      wxWindow:refresh(Frame),
      {noreply, State#state{file=LocalFile}};

    true ->
      wxTextCtrl:changeValue(Log, "Wrong File"),
      wxWindow:refresh(Frame),
      {noreply, State}

  end;



handle_event(#wx{obj = Button, event = #wxCommand{type = command_button_clicked}},
    State = #state{     algchooser=Choice,
      master=M,
      sourcechooser=StartPicker,
      destchooser=EndPicker,
      filechooser=FilePicker,
      node1=TextCtrl1,
      log=Log,
      frame=Parent,
      panel=Panel,clicked=Click
    }) ->
  SMList=string:split(wxTextCtrl:getValue(TextCtrl1),","),
  File = wxFilePickerCtrl:getPath(FilePicker),
  io:format("File is : ~p", [File]),
  FileL = length(lists:flatten(File)),
  if
    (FileL > 1) and (Click==0) ->
      Tokens=string:tokens(File,"/"),
      LocalFile=lists:last(Tokens),
      Root = wxSpinCtrl:getValue(StartPicker),
      Dest =wxSpinCtrl:getValue(EndPicker),
      SP=wxListBox:isSelected(Choice,0),
      MST =wxListBox:isSelected(Choice,1),
      SPT =wxListBox:isSelected(Choice,2),
      io:format("Starting alg",[]),
      io:format("SMList : ~p", [SMList]),
	ClickA=1,
      active_alg(M,ping_sm(SMList), LocalFile,Root, Dest, SP,MST,SPT,Log);
    true ->
      wxTextCtrl:changeValue(Log, "Wrong File"),ClickA=0
  end,
%active_alg(SMList, LocalFile,Root, Dest, SP,MST,SPT,Log);

%.

  {noreply, State#state{clicked=ClickA}};




%% Re-Paint Event - called by refresh


%%map choose event


%% speed update event
%% create m_drone button event

%% remove all drones button event


%% list of drones selection event


handle_event(#wx{event = #wxClose{}},
    State = #state{ frame=Frame , panel=Panel
    }) ->
wxFrame:destroy(Frame),
  io:format("Exiting~n"),
  io:format("Exiting~n"),
  io:format("Exiting~n"),
  io:format("Exiting~n"),
  io:format("Exiting~n"),
  {stop,normal,State};

handle_event(_Ev = #wx{}, State = #state{}) ->
  io:format("Got Event ~n"),
  {noreply, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(Msg, State) ->
  io:format("Got Info ~p~n",[Msg]),
  {noreply, State}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply,Reply,State}.

%%data Rx from drones

handle_cast({done,Outputs},
    State = #state{algchooser = Choice ,frame = Frame,log = Log}) ->
wxTextCtrl:changeValue(Log, ".  "),
SP=wxListBox:isSelected(Choice,0),
MST=wxListBox:isSelected(Choice,1),
BFS=wxListBox:isSelected(Choice,2),
if
SP == true -> {Runtime, Iterations, Path, Weight}=Outputs, wxTextCtrl:writeText(Log, lists:flatten(io_lib:format("Runtime: ~p , Iterations: ~p, Path: ~p  , Weight: ~p", [Runtime,Iterations,Path,Weight])));
MST == true -> {Weight,Iterations,Runtime} = Outputs,wxTextCtrl:writeText(Log, lists:flatten(io_lib:format("Runtime: ~p , Iterations: ~p,  Weight: ~p", [Runtime,Iterations,Weight]))) ;
BFS == true -> {Runtime,Iterations,MeanDest} = Outputs,wxTextCtrl:writeText(Log, lists:flatten(io_lib:format("Runtime: ~p , Iterations: ~p,  Mean Distance : ~p", [Runtime,Iterations,MeanDest]))) ;
true->ok
end,


  wxPanel:refresh(Frame),
  {noreply,State#state{clicked=0}};



handle_cast(Msg, State) ->
%io:format("nndddsaddsaasfdfsafas~p~n" , [Msg]),

  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

active_alg(M,SMList, LocalFile,Root, Dest, SP,MST,SPT,Log) ->
  io:format("The SMList is: ~p. File is : ~p. Root is ~p. Dest is ~p. SP is ~p. MST is ~p . SPT is ~p.", [SMList, LocalFile,Root, Dest, SP,MST,SPT]),
  Alg_dec= decodeAlg(SP,MST,SPT),
  sendgo(M, Alg_dec, SMList, LocalFile, Root, Dest,Log).





ping_sm([])->[];
ping_sm([H | T] ) ->
  NodeL = length(lists:flatten(H)),

  if
    NodeL > 1 -> [ping_it(H)]++ ping_sm(T);
    true -> ping_sm(T)
  end.
ping_it(NodeAd)->
io:format("send ping to : ~p", [NodeAd]),
  Result = net_adm:ping(list_to_atom(NodeAd)),
  
  if
    Result==pong -> list_to_atom(NodeAd);
    true -> []
  end.


decodeAlg(SP,MST,SPT)->
  if
    SP==true -> bellman;
    MST==true -> mst;
    SPT==true -> bfs;
    true ->mst
  end.

sendgo(M, bellman, SMList, LocalFile, Root, Dest,_Log)->
gen_statem:call(M,{bellman, SMList, LocalFile,{Root, Dest}}),
  io:format("call master : ~p", [M]);

  %gen_statem:call(M,{bellman, ['submaster1@Jonathans-Air'], "mst.txt",{1, 2}}).

sendgo(M, mst, SMList, LocalFile, Root, Dest,_Log)->
gen_statem:call(M,{mst, SMList, LocalFile,Root}),
  io:format("call master : ~p", [M]);

sendgo(M, bfs, SMList, LocalFile, Root, _Dest,_Log)->
gen_statem:call(M,{bfs, SMList, LocalFile,Root}),
  io:format("call master : ~p", [M]).

init_gui(Mode,Node) ->
%register(gui,self()),
  io:format("my pid ~p.", [self()]),

  wx:new(),
  Choices = ["Shortest path between vertex","Minimum spanning tree","Shortest paths tree"],
  GParent = wxWindow:new(),
  Parent = wxFrame:new(GParent, 1, "Graph Algorithms" ,[{size,{600, 600}}]),
  Panel = wxPanel:new(Parent, []),

  %% Setup sizers

  FirstNodeSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "Nodes list, separated by ','"}]),









  MainSizer = wxBoxSizer:new(?wxVERTICAL),
  FilePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "Input File"}]),
  ChoicePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "Choose algorithm"}]),
  StartPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "Start/Root Vertex (if needed)"}]),
  EndPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "End Vertex (if needed)"}]),
  ButtonPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "Start work"}]),
  %ClosePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,[{label, "Close"}]),

  LogSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel,
    [{label, "log"}]),

  TextCtrl1  = wxTextCtrl:new(Panel, 5, [{value, ""},
    {style, ?wxDEFAULT}]),


  FilePicker = wxFilePickerCtrl:new(Panel, 1, [{path, "/"}]),
  Choice = wxListBox:new(Panel, 7, [{choices, Choices}]),
  StartPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(StartPicker, 1, 1000000000),
  EndPicker = wxSpinCtrl:new(Panel, []),
  wxSpinCtrl:setRange(EndPicker, 1, 1000000000),
  Button = wxButton:new(Panel, 10, [{label, "Start"}]),
  %Close = wxButton:new(Panel, 10, [{label, "Close"}]),
  Log = wxTextCtrl:new(Panel, 7, [{value, "Logging"},
    {style, ?wxDEFAULT}]),






  wxFilePickerCtrl:connect(FilePicker, command_filepicker_changed, []),
  wxFrame:connect(Parent,close_window),

  %wxButton:connect(Close, command_button_clicked, [{callback, fun handle_click2/2}, {userData, #{parent => Parent,env => wx:get_env()}}]),
  wxButton:connect(Button,command_button_clicked),

  %% Add to sizers
  PickerOptions = [{border, 1},{flag, ?wxALL bor ?wxEXPAND}],
  wxSizer:add(FilePickerSizer, FilePicker, PickerOptions),
  wxSizer:add(StartPickerSizer, StartPicker, PickerOptions),
  wxSizer:add(EndPickerSizer, EndPicker, PickerOptions),
  wxSizer:add(LogSizer, Log, PickerOptions),
  wxSizer:add(ButtonPickerSizer, Button, PickerOptions),
  %wxSizer:add(ClosePickerSizer, Close, PickerOptions),
  wxSizer:add(FirstNodeSizer, TextCtrl1, PickerOptions),


  wxSizer:add(ChoicePickerSizer, Choice, PickerOptions),


  SizerOptions  = [{flag, ?wxEXPAND}],
  wxSizer:add(MainSizer, ButtonPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, FilePickerSizer, SizerOptions),
  wxSizer:add(MainSizer, ChoicePickerSizer, SizerOptions),
  wxSizer:add(MainSizer, FirstNodeSizer, SizerOptions),


  wxSizer:add(MainSizer, StartPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, EndPickerSizer, SizerOptions),
  wxSizer:add(MainSizer, LogSizer, SizerOptions),

  wxSizer:add(MainSizer, ButtonPickerSizer, SizerOptions),
  %wxSizer:add(MainSizer, ClosePickerSizer, SizerOptions),
  wxPanel:setSizer(Panel, MainSizer),
  {ok,M}=master:start_link(),
  wxFrame:show(Parent),
  #state{
    clicked=0,
    algchooser=Choice,
    sourcechooser=StartPicker,
    destchooser=EndPicker,
    filechooser=FilePicker,
    file='1.txt',
    node1=TextCtrl1,
    log=Log,
    frame=Parent,
    master=M,

    panel=Panel
  }.
