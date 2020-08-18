-module(guii).
-export([start/0,bla/0]).
-include_lib("wx/include/wx.hrl").
-record(state, 
	{
	  parent,
	  config
	}).
bla()->


  wx:new(),
  
Parent = wxFrame:new(wx:null(), 2, "Graph Algorithms" ,[{size,{600, 600}}]),
Panel = wxPanel:new(Parent, []),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),

    ButtonPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "Start work"}]),

    Button2 = wxButton:new(Panel, 7, [{label, "Start"}]),





    wxButton:connect(Button2, command_button_clicked, [{callback, fun handle_click/2}, {userData, #{env => wx:get_env()}}]),
    PickerOptions = [{border, 4},{flag, ?wxALL bor ?wxEXPAND}],

    wxSizer:add(ButtonPickerSizer, Button2, PickerOptions),
    SizerOptions  = [{flag, ?wxEXPAND}],
    wxSizer:add(MainSizer, ButtonPickerSizer, SizerOptions),
    wxPanel:setSizer(Panel, MainSizer),
    wxFrame:show(Parent).


start() ->
  wx:new(),
    Choices = ["Shortest path between vertex","Minimum spanning tree","Shortest paths tree"],

  Parent = wxFrame:new(wx:null(), 1, "Graph Algorithms" ,[{size,{600, 600}}]),
Panel = wxPanel:new(Parent, []),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    FirstNodeSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "First node"}]),
    SecondNodeSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "Second node"}]),
    ThirdNodeSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "Third node"}]),
    ForthNodeSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "Forth node"}]),
    ButtonPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "Start work"}]),


    TextCtrl1  = wxTextCtrl:new(Panel, 1, [{value, "This is a single line wxTextCtrl"},
					 {style, ?wxDEFAULT}]),
    TextCtrl2  = wxTextCtrl:new(Panel, 2, [{value, "This is a single line wxTextCtrl"},
					 {style, ?wxDEFAULT}]),
    TextCtrl3  = wxTextCtrl:new(Panel, 3, [{value, "This is a single line wxTextCtrl"},
					 {style, ?wxDEFAULT}]),
    TextCtrl4  = wxTextCtrl:new(Panel, 4, [{value, "This is a single line wxTextCtrl"},
					 {style, ?wxDEFAULT}]),

    Button = wxButton:new(Panel, 7, [{label, "Start"}]),



    wxButton:connect(Button, command_button_clicked, [{callback, fun handle_click_first/2}, {userData, #{txt1 => TextCtrl1, txt2 => TextCtrl2 , txt3 => TextCtrl3 , txt4 => TextCtrl4, env => wx:get_env()}}]),

    %% Add to sizers
    PickerOptions = [{border, 4},{flag, ?wxALL bor ?wxEXPAND}],
    wxSizer:add(FirstNodeSizer, TextCtrl1, PickerOptions),
    wxSizer:add(SecondNodeSizer, TextCtrl2, PickerOptions),
    wxSizer:add(ThirdNodeSizer, TextCtrl3, PickerOptions),
    wxSizer:add(ForthNodeSizer, TextCtrl4, PickerOptions),
    wxSizer:add(ButtonPickerSizer, Button, PickerOptions),

    SizerOptions  = [{flag, ?wxEXPAND}],
    wxSizer:add(MainSizer, FirstNodeSizer, SizerOptions),
    wxSizer:add(MainSizer, SecondNodeSizer, SizerOptions),
    wxSizer:add(MainSizer, ThirdNodeSizer, SizerOptions),
    wxSizer:add(MainSizer, ForthNodeSizer, SizerOptions),
    wxSizer:add(MainSizer, ButtonPickerSizer, SizerOptions),
    wxPanel:setSizer(Panel, MainSizer),
    wxFrame:show(Parent).



handle_click_first(#wx{obj = Button, userData = #{txt1 := TextCtrl1 , txt2 := TextCtrl2 , txt3 := TextCtrl3 , txt4 := TextCtrl4 , env := Env}}, _Event) ->
  wx:set_env(Env),
  io:format("The value is: ~p", [wxTextCtrl: getValue(TextCtrl1)]).





main_window(SMlist) ->
  wx:new(),
    Choices = ["Shortest path between vertex","Minimum spanning tree","Shortest paths tree"],

  Parent = wxFrame:new(wx:null(), 1, "Graph Algorithms" ,[{size,{600, 600}}]),
Panel = wxPanel:new(Parent, []),

    %% Setup sizers
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    FilePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "Input File"}]),
    DirPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "Output Folder"}]),
    ChoicePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "Choose algorithm"}]),
    StartPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "Start/Root Vertex (if needed)"}]),
    EndPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "End Vertex (if needed)"}]),
    DatePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "wxDatePickerCtrl"}]),
    ButtonPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "Start work"}]),


    FilePicker = wxFilePickerCtrl:new(Panel, 1, [{path, "/"}]),
    DirPicker = wxDirPickerCtrl:new(Panel, 2, [{path, "/"}]),
    Choice = wxListBox:new(Panel, 3, [{choices, Choices}]),
    StartPicker = wxSpinCtrl:new(Panel, []),
    wxSpinCtrl:setRange(StartPicker, 0, 1000000000),
    EndPicker = wxSpinCtrl:new(Panel, []),
    wxSpinCtrl:setRange(EndPicker, 0, 1000000000),
    DatePicker = wxDatePickerCtrl:new(Panel, 6, []),
    Button = wxButton:new(Panel, 7, [{label, "Start"}]),




    wxFilePickerCtrl:connect(FilePicker, command_filepicker_changed, []),
    wxDirPickerCtrl:connect(DirPicker, command_dirpicker_changed, []),
    %wxFontPickerCtrl:connect(FontPicker, command_fontpicker_changed, []),
    wxDatePickerCtrl:connect(DatePicker, date_changed, []),
    wxButton:connect(Button, command_button_clicked, [{callback, fun handle_click/2}, {userData, #{value => DirPicker, algo=> Choice , panel=> Panel, env => wx:get_env()}}]),

    %% Add to sizers
    PickerOptions = [{border, 4},{flag, ?wxALL bor ?wxEXPAND}],
    wxSizer:add(FilePickerSizer, FilePicker, PickerOptions),
    wxSizer:add(DirPickerSizer, DirPicker, PickerOptions),
    wxSizer:add(StartPickerSizer, StartPicker, PickerOptions),
    wxSizer:add(EndPickerSizer, EndPicker, PickerOptions),
    wxSizer:add(DatePickerSizer, DatePicker, PickerOptions),
    wxSizer:add(ButtonPickerSizer, Button, PickerOptions),
    wxSizer:add(ChoicePickerSizer, Choice, PickerOptions),

    SizerOptions  = [{flag, ?wxEXPAND}],
    wxSizer:add(MainSizer, FilePickerSizer, SizerOptions),
    wxSizer:add(MainSizer, DirPickerSizer, SizerOptions),
    wxSizer:add(MainSizer, ChoicePickerSizer, SizerOptions),
    wxSizer:add(MainSizer, StartPickerSizer, SizerOptions),
    wxSizer:add(MainSizer, EndPickerSizer, SizerOptions),
    wxSizer:add(MainSizer, DatePickerSizer, SizerOptions),
    wxSizer:add(MainSizer, ButtonPickerSizer, SizerOptions),
    wxPanel:setSizer(Panel, MainSizer),
    wxFrame:show(Parent).


handle_click(#wx{obj = Button, userData = #{value := DirPicker , algo := Choice ,panel:=Panel , env := Env}}, _Event) ->
  wx:set_env(Env),
  io:format("The value is: ~p.", [wxDirPickerCtrl:getPath(DirPicker)]),
  io:format("The value is: ~p.", [wxListBox:getSelections(Choice)]);



handle_click(#wx{obj = Button2, userData = #{env := Env}}, _Event) ->
 start(). 