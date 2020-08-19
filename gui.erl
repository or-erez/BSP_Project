-module(gui).
-export([start/0]).
-include_lib("wx/include/wx.hrl").

start() ->
  wx:new(),
    Choices = ["Shortest path between vertex","Minimum spanning tree","Shortest paths tree"],
GParent = wxWindow:new(),
  Parent = wxFrame:new(GParent, 1, "Graph Algorithms" ,[{size,{600, 800}}]),
Panel = wxPanel:new(Parent, []),

    %% Setup sizers

    FirstNodeSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "First node"}]),
    SecondNodeSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "Second node"}]),
    ThirdNodeSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "Third node"}]),
    ForthNodeSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "Forth node"}]),








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
    ClosePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "Close"}]),

    LogSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "log"}]),

    TextCtrl1  = wxTextCtrl:new(Panel, 5, [{value, ""},
					 {style, ?wxDEFAULT}]),
    TextCtrl2  = wxTextCtrl:new(Panel, 2, [{value, ""},
					 {style, ?wxDEFAULT}]),
    TextCtrl3  = wxTextCtrl:new(Panel, 3, [{value, ""},
					 {style, ?wxDEFAULT}]),
    TextCtrl4  = wxTextCtrl:new(Panel, 4, [{value, ""},
					 {style, ?wxDEFAULT}]),

    FilePicker = wxFilePickerCtrl:new(Panel, 1, [{path, "/"}]),
    Choice = wxListBox:new(Panel, 7, [{choices, Choices}]),
    StartPicker = wxSpinCtrl:new(Panel, []),
    wxSpinCtrl:setRange(StartPicker, 8, 1000000000),
    EndPicker = wxSpinCtrl:new(Panel, []),
    wxSpinCtrl:setRange(EndPicker, 9, 1000000000),
    Button = wxButton:new(Panel, 10, [{label, "Start"}]),
    Close = wxButton:new(Panel, 10, [{label, "Close"}]),
    Log = wxTextCtrl:new(Panel, 7, [{value, "Logging"},
					  {style, ?wxDEFAULT}]),






    wxFilePickerCtrl:connect(FilePicker, command_filepicker_changed, []),
    wxButton:connect(Button, command_button_clicked, [{callback, fun handle_click/2}, {userData, #{algo=> Choice ,parent => Parent, panel=> Panel, file => FilePicker,root => StartPicker,dest => EndPicker, log=>Log, txt1 => TextCtrl1, txt2 => TextCtrl2 , txt3 => TextCtrl3 , txt4=> TextCtrl4,env => wx:get_env()}}]),

    wxButton:connect(Close, command_button_clicked, [{callback, fun handle_click2/2}, {userData, #{parent => Parent,env => wx:get_env()}}]),

    %% Add to sizers
    PickerOptions = [{border, 1},{flag, ?wxALL bor ?wxEXPAND}],
    wxSizer:add(FilePickerSizer, FilePicker, PickerOptions),
    wxSizer:add(StartPickerSizer, StartPicker, PickerOptions),
    wxSizer:add(EndPickerSizer, EndPicker, PickerOptions),
    wxSizer:add(LogSizer, Log, PickerOptions),
    wxSizer:add(ButtonPickerSizer, Button, PickerOptions),
    wxSizer:add(ClosePickerSizer, Close, PickerOptions),
    wxSizer:add(FirstNodeSizer, TextCtrl1, PickerOptions),
    wxSizer:add(SecondNodeSizer, TextCtrl2, PickerOptions),
    wxSizer:add(ThirdNodeSizer, TextCtrl3, PickerOptions),
    wxSizer:add(ForthNodeSizer, TextCtrl4, PickerOptions),

    wxSizer:add(ChoicePickerSizer, Choice, PickerOptions),


    SizerOptions  = [{flag, ?wxEXPAND}],
    wxSizer:add(MainSizer, ButtonPickerSizer, SizerOptions),
    wxSizer:add(MainSizer, FilePickerSizer, SizerOptions),
    wxSizer:add(MainSizer, ChoicePickerSizer, SizerOptions),
    wxSizer:add(MainSizer, FirstNodeSizer, SizerOptions),
    wxSizer:add(MainSizer, SecondNodeSizer, SizerOptions),
    wxSizer:add(MainSizer, ThirdNodeSizer, SizerOptions),
    wxSizer:add(MainSizer, ForthNodeSizer, SizerOptions),

    wxSizer:add(MainSizer, StartPickerSizer, SizerOptions),
    wxSizer:add(MainSizer, EndPickerSizer, SizerOptions),
    wxSizer:add(MainSizer, LogSizer, SizerOptions),

    wxSizer:add(MainSizer, ButtonPickerSizer, SizerOptions),
    wxSizer:add(MainSizer, ClosePickerSizer, SizerOptions),
    wxPanel:setSizer(Panel, MainSizer),
    wxFrame:show(Parent).


handle_click(#wx{obj = _Button, userData = #{algo := Choice , parent := _Parent ,panel:=_Panel , file := FilePicker  ,root := StartPicker, dest := EndPicker ,log:=Log, txt1 := TextCtrl1  , txt2 := TextCtrl2 , txt3 := TextCtrl3 , txt4:= TextCtrl4 ,env := Env}}, _Event) ->
	wx:set_env(Env),
	SMList=[wxTextCtrl:getValue(TextCtrl1),wxTextCtrl:getValue(TextCtrl2),wxTextCtrl:getValue(TextCtrl3),wxTextCtrl:getValue(TextCtrl4)],
File = wxFilePickerCtrl:getPath(FilePicker),
io:format("File is : ~p", [File]),
FileL = length(lists:flatten(File)),
if
FileL > 1 ->
Tokens=string:tokens(File,"/"),
LocalFile=lists:last(Tokens),
wxTextCtrl:changeValue(Log, "a"),
Root = wxSpinCtrl:getValue(StartPicker),
Dest =wxSpinCtrl:getValue(EndPicker),
SP=wxListBox:isSelected(Choice,0),
MST =wxListBox:isSelected(Choice,1),
SPT =wxListBox:isSelected(Choice,2),
io:format("Starting alg",[]),
io:format("SMList : ~p", [SMList]),
active_alg(ping_sm(SMList), LocalFile,Root, Dest, SP,MST,SPT,Log);
true ->
wxTextCtrl:changeValue(Log, "Wrong File")
end.
%active_alg(SMList, LocalFile,Root, Dest, SP,MST,SPT,Log);

%.



handle_click2(#wx{obj = _Close, userData = #{parent := Parent, env := Env}}, _Event) ->

  wx:set_env(Env),
wxFrame:destroy(Parent).

active_alg(SMList, LocalFile,Root, Dest, SP,MST,SPT,_Log) ->
io:format("The SMList is: ~p. File is : ~p. Root is ~p. Dest is ~p. SP is ~p. MST is ~p . SPT is ~p.", [SMList, LocalFile,Root, Dest, SP,MST,SPT]).

ping_sm([])->[];
ping_sm([H | T] ) -> 
NodeL = length(lists:flatten(H)),
if 
  NodeL > 1 -> [ping_it(H)]++ ping_sm([T]);
  true -> ping_sm(T)
end.
ping_it(NodeAd)->
Result = net_adm:ping(list_to_atom(NodeAd)),
 io:format("send ping to : ~p", [NodeAd]),
if
	Result==pong -> NodeAd;
	true -> []
end.