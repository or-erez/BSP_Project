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
%%%%handle file choosing event



handle_event(#wx{obj = FilePicker, event = #wxCommand{type = command_filepicker_changed}},
    State = #state{frame = Frame , log = Log}) ->
  File = wxFilePickerCtrl:getPath(FilePicker),
  FileL = length(lists:flatten(File)),
  if
    FileL > 1 -> %check if it is a valid file
      Tokens=string:tokens(File,"/"),
      LocalFile=lists:last(Tokens),
      wxWindow:refresh(Frame),
      {noreply, State#state{file=LocalFile}};

    true ->
      wxTextCtrl:changeValue(Log, "Wrong File"),
      wxWindow:refresh(Frame),
      {noreply, State}

  end;


%%handle 'start' event
handle_event(#wx{obj = _Button, event = #wxCommand{type = command_button_clicked}},
    State = #state{     algchooser=Choice,
      master=M,
      sourcechooser=StartPicker,
      destchooser=EndPicker,
      filechooser=FilePicker,
      node1=TextCtrl1,
      log=Log,
      frame=_Parent,
      panel=_Panel,clicked=Click
    }) ->
  SMList=string:split(wxTextCtrl:getValue(TextCtrl1),",",all), %get the submaster list and tokenize them
  File = wxFilePickerCtrl:getPath(FilePicker), %get the chosen file
  io:format("File is : ~p", [File]),
  FileL = length(lists:flatten(File)),
  if
    (FileL > 1) and (Click==0) -> %check if the previous run was completed and the file is valid
      Tokens=string:tokens(File,"/"),
      LocalFile=lists:last(Tokens),
      Root = wxSpinCtrl:getValue(StartPicker), %get the Root
      Dest =wxSpinCtrl:getValue(EndPicker), %get the dest
      SP=wxListBox:isSelected(Choice,0), %check if the user chose Bellman Ford as the algorithm
      MST =wxListBox:isSelected(Choice,1), %check if the user chose MST as the algorithm
      SPT =wxListBox:isSelected(Choice,2), %check if the user chose BFS as the algorithm
      io:format("Starting alg",[]),
      io:format("SMList : ~p", [SMList]),
	ClickA=1,
      active_alg(M,ping_sm(SMList), LocalFile,Root, Dest, SP,MST,SPT,Log);
    true ->
      wxTextCtrl:changeValue(Log, "Wrong File"),ClickA=0 %notify the user that he picked wrong file
  end,
%active_alg(SMList, LocalFile,Root, Dest, SP,MST,SPT,Log);

%.

  {noreply, State#state{clicked=ClickA}};

handle_event(#wx{event = #wxClose{}}, %close window event, handle memory leak
    State = #state{ frame=Frame , panel=_Panel
    }) ->
wxFrame:destroy(Frame),
  io:format("exit~n"),
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

handle_cast({done,Outputs}, %handle completion call from the master
    State = #state{algchooser = Choice ,frame = Frame,log = Log}) ->
wxTextCtrl:changeValue(Log, ".  "), %clean the log
SP=wxListBox:isSelected(Choice,0), %chose what output to print
MST=wxListBox:isSelected(Choice,1),
BFS=wxListBox:isSelected(Choice,2),
if
SP == true -> {Runtime, Iterations, Path, Weight}=Outputs, wxTextCtrl:writeText(Log, lists:flatten(io_lib:format("Runtime: ~p , Iterations: ~p, Path: ~p  , Weight: ~p", [Runtime,Iterations,Path,Weight])));
MST == true -> {Weight,Iterations,Runtime} = Outputs,wxTextCtrl:writeText(Log, lists:flatten(io_lib:format("Runtime: ~p , Iterations: ~p,  Weight: ~p", [Runtime,Iterations,Weight]))) ;
BFS == true -> {Runtime,Iterations,MeanDest} = Outputs,wxTextCtrl:writeText(Log, lists:flatten(io_lib:format("Runtime: ~p , Iterations: ~p,  Mean Distance : ~p", [Runtime,Iterations,MeanDest]))) ;
true->ok
end,


  wxPanel:refresh(Frame), %refresh the panel
  {noreply,State#state{clicked=0}};



handle_cast(_Msg, State) ->

  {noreply,State}.

code_change(_, _, State) ->
  {stop, ignore, State}.

terminate(_Reason, _) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%active the algorithm 
active_alg(M,SMList, LocalFile,Root, Dest, SP,MST,SPT,Log) ->
  io:format("The SMList is: ~p. File is : ~p. Root is ~p. Dest is ~p. SP is ~p. MST is ~p . SPT is ~p.", [SMList, LocalFile,Root, Dest, SP,MST,SPT]),
  Alg_dec= decodeAlg(SP,MST,SPT),
  sendgo(M, Alg_dec, SMList, LocalFile, Root, Dest,Log).




%check the validity of the submasters nodes 
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

%check what algorithm was chosen
decodeAlg(SP,MST,SPT)->
  if
    SP==true -> bellman;
    MST==true -> mst;
    SPT==true -> bfs;
    true ->mst
  end.

%call the submaster to start (Bellman Ford)
sendgo(M, bellman, SMList, LocalFile, Root, Dest,_Log)->
gen_statem:call(M,{bellman, SMList, LocalFile,{Root, Dest}}),
  io:format("call master : ~p", [M]);

%call the submaster to start (MST)

sendgo(M, mst, SMList, LocalFile, Root, _Dest,_Log)->
gen_statem:call(M,{mst, SMList, LocalFile,Root}),
  io:format("call master : ~p", [M]);

%call the submaster to start (BFS)

sendgo(M, bfs, SMList, LocalFile, Root, _Dest,_Log)->
gen_statem:call(M,{bfs, SMList, LocalFile,Root}),
  io:format("call master : ~p", [M]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% We used wx:demo() for wx widgets templates and examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_gui(_Mode,_Node) ->
  io:format("my pid ~p.", [self()]),

  wx:new(),
  Choices = ["Shortest path between vertex","Minimum spanning tree","Shortest paths tree"], %supported algorithms
  GParent = wxWindow:new(),
  Parent = wxFrame:new(GParent, 1, "Graph Algorithms" ,[{size,{600, 600}}]), %create frame
  Panel = wxPanel:new(Parent, []), %create panel

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






  wxFilePickerCtrl:connect(FilePicker, command_filepicker_changed, []), %connect between chosing file and the handler
  wxFrame:connect(Parent,close_window), %connect between closing the window and the handler
  wxButton:connect(Button,command_button_clicked), %connect between start click and the handler

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
