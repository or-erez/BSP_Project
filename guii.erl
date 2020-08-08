-module(guii).
-export([start/0,bla/0]).
-include_lib("wx/include/wx.hrl").
-record(state, 
	{
	  parent,
	  config
	}).
bla()->
code:lib_dir().
start() ->
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

    FontPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					  [{label, "wxFontPickerCtrl"}]),
    DatePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "wxDatePickerCtrl"}]),
    ButtonPickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "Start work"}]),
    ChoicePickerSizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
					   [{label, "Choose algorithm"}]),

    FilePicker = wxFilePickerCtrl:new(Panel, 1, [{path, "/"}]),
    DirPicker = wxDirPickerCtrl:new(Panel, 2, [{path, "/"}]),
    FontPicker = wxFontPickerCtrl:new(Panel, 3, []),
    DatePicker = wxDatePickerCtrl:new(Panel, 4, []),
    Button = wxButton:new(Panel, 5, [{label, "Start"}]),

    Choice = wxListBox:new(Panel, 6, [{choices, Choices}]),



    wxFilePickerCtrl:connect(FilePicker, command_filepicker_changed, []),
    wxDirPickerCtrl:connect(DirPicker, command_dirpicker_changed, []),
    wxFontPickerCtrl:connect(FontPicker, command_fontpicker_changed, []),
    wxDatePickerCtrl:connect(DatePicker, date_changed, []),
    wxButton:connect(Button, command_button_clicked, [{callback, fun handle_click/2}, {userData, #{value => DirPicker, algo=> Choice , panel=> Panel, env => wx:get_env()}}]),

    %% Add to sizers
    PickerOptions = [{border, 4},{flag, ?wxALL bor ?wxEXPAND}],
    wxSizer:add(FilePickerSizer, FilePicker, PickerOptions),
    wxSizer:add(DirPickerSizer, DirPicker, PickerOptions),
    wxSizer:add(FontPickerSizer, FontPicker, PickerOptions),
    wxSizer:add(DatePickerSizer, DatePicker, PickerOptions),
    wxSizer:add(ButtonPickerSizer, Button, PickerOptions),
    wxSizer:add(ChoicePickerSizer, Choice, PickerOptions),

    SizerOptions  = [{flag, ?wxEXPAND}],
    wxSizer:add(MainSizer, FilePickerSizer, SizerOptions),
    wxSizer:add(MainSizer, DirPickerSizer, SizerOptions),
    wxSizer:add(MainSizer, ChoicePickerSizer, SizerOptions),
    wxSizer:add(MainSizer, FontPickerSizer, SizerOptions),
    wxSizer:add(MainSizer, DatePickerSizer, SizerOptions),
    wxSizer:add(MainSizer, ButtonPickerSizer, SizerOptions),









    wxPanel:setSizer(Panel, MainSizer),
  wxFrame:show(Parent).


handle_click(#wx{obj = Button, userData = #{value := DirPicker , algo := Choice ,panel:=Panel , env := Env}}, _Event) ->
  wx:set_env(Env),
  io:format("The value is: ~p.", [wxDirPickerCtrl:getPath(DirPicker)]),
  io:format("The value is: ~p.", [wxListBox:getSelections(Choice)]),
wxWindow:popupMenu(Panel, create_menu()). 

handle_event(#wx{event = #wxFileDirPicker{type = command_filepicker_changed,
					  path = Path}},
	     State = #state{}) ->
 bla.
    



create_menu() ->
    Menu = wxMenu:new([]),
    SubMenu  = wxMenu:new([]),
    SubMenu2 = wxMenu:new([]),

    wxMenu:append(Menu, ?wxID_UNDO, "Undo", []),
    wxMenu:append(Menu, ?wxID_REDO, "Redo", []),
    wxMenu:append(Menu, ?wxID_HELP, "Help", []),
    wxMenu:appendSeparator(Menu),
    wxMenu:appendCheckItem(Menu, ?wxID_ANY, "Check item", []),
    wxMenu:appendSeparator(Menu),
    wxMenu:appendRadioItem(Menu, ?wxID_ANY, "Radio item 1", []),
    wxMenu:appendRadioItem(Menu, ?wxID_ANY, "Radio item 2", []),
    wxMenu:appendRadioItem(Menu, ?wxID_ANY, "Radio item 3", []),
    wxMenu:appendRadioItem(Menu, ?wxID_ANY, "Radio item 4", []),

    wxMenu:appendSeparator(Menu),
    wxMenuItem:enable(wxMenu:append(Menu, ?wxID_ANY, "Disabled", []), [{enable,false}]),
    wxMenu:appendSeparator(Menu),

    wxMenu:append(SubMenu, ?wxID_ABOUT, "About", []),
    wxMenu:append(SubMenu, ?wxID_ANY, "Sub Item2", []),
    wxMenu:append(SubMenu, ?wxID_SAVE, "Save", []),
    wxMenu:break(SubMenu),
    wxMenu:append(SubMenu, ?wxID_EXIT, "Exit", []),
    wxMenu:append(SubMenu, ?wxID_OPEN, "Open", []),
    wxMenu:append(SubMenu, ?wxID_NEW, "New", []),
    wxMenu:append(Menu, ?wxID_ANY, "Sub menu", SubMenu, []),

    wxMenu:appendCheckItem(SubMenu2, ?wxID_ANY, "Check Item", []),
    wxMenu:appendSeparator(SubMenu2),
    wxMenu:append(SubMenu2, ?wxID_CLEAR, "Clear", []),
    wxMenu:append(SubMenu2, ?wxID_ANY, "Sub Item", []),

    Bitmap = wxArtProvider:getBitmap("wxART_NEW"),
    AnotherSubMenu = wxMenuItem:new([{parentMenu, Menu},
				     {id, ?wxID_ANY},
				     {text, "Another sub menu"},
				     {subMenu, SubMenu2},
				     {kind, ?wxITEM_NORMAL}]),
    wxMenuItem:setBitmap(AnotherSubMenu, Bitmap),
    wxMenu:append(Menu, AnotherSubMenu),

    wxMenu:connect(Menu, command_menu_selected),
    wxMenu:connect(SubMenu, command_menu_selected),
    wxMenu:connect(SubMenu2, command_menu_selected),
    Menu.
