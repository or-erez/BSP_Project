%%%-------------------------------------------------------------------
%%% @author Or
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2020 7:37 PM
%%%-------------------------------------------------------------------
-module(master).
-author("Or").

-behaviour(gen_statem).

%% API
-export([start_link/0, name/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

%state functions
-export([idle/3,giveOrders/3,analyze/3]).

-define(SERVER, ?MODULE).

-record(master_state, {alg, num_SM, iter, range_list,m_supp_data, armed_SM_counter, dims, filepath, algdata, outputs}).

name() -> master.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
  io:format("Server start with pid: ~p , node: ~p~n", [self(), node()]),
  {ok, idle, #master_state{alg = a, num_SM = 0, iter = 0, range_list = [], m_supp_data = a, armed_SM_counter = 0, dims = 0, filepath = null, algdata = null, outputs = null}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.

%%Initial state, actions occur upon call sent from GUI. It is assumed arguments are legal
idle({call,From}, {Alg,SMList,FilePath,AlgData}, State = #master_state{}) ->
  %io:format("master received call, Alg:~p ~n", [Alg]),
  ActiveList = requestSM(Alg,SMList), %pings all sub-masters, to create a list of active ones.
  Dims = checkGraphDims(FilePath), %reads graph once, to learn the dimensions and store in state
  RangeList = splitRange(ActiveList,Dims), %splits the graph into ranges
  NumSM = length(RangeList),
  {SMData,MData, Outputs} = prepAlg(Alg,State,AlgData), %algorithm specific preperation
  initiateSM(FilePath,RangeList,SMData), %sends the initiating message to the submasters, prompting their setup
  %io:format("switching to giveOrders: ~n", []),
  {next_state, giveOrders,
    State#master_state{alg = Alg, range_list = RangeList, m_supp_data = MData, num_SM = NumSM,dims = Dims, filepath = FilePath, algdata = AlgData,  armed_SM_counter = 0, outputs = Outputs},
    [{reply, From, ack},{state_timeout,10000,awol_sm}]}.


%When all submasters respond that they finished, the master gives the initial orders
giveOrders(cast, _Response, State = #master_state{}) ->
  %io:format("giveOrders, the cast was:~p  ~n", [Response]),
  Next_Counter = State#master_state.armed_SM_counter + 1,
  if (State#master_state.armed_SM_counter < (State#master_state.num_SM-1)) -> %more submasters remain
    {keep_state, State#master_state{armed_SM_counter = Next_Counter}};
  true ->
    SMData = prepGo(State#master_state.alg,State), %algorithm specific first iteration preperation
    sendGos(State#master_state.range_list,1,SMData), %sends out iteration to submasters
    {next_state, analyze, State#master_state{armed_SM_counter = Next_Counter, iter = 1},{timeout,State#master_state.dims+1000,awol_sm}} end;

%a submaster timed out, algorithm is reset
giveOrders(state_timeout,awol_sm,State) ->
  killSM(State#master_state.range_list),
  %%flush(),
  NewState = resetAlg(State),
  {next_state, giveOrders, NewState,{state_timeout,10000,awol_sm}}.




%%This state includes the entire calculation of the algorithm. Action is triggered every time a submaster is done.
analyze(cast, {completion, SMData}, State = #master_state{}) ->
  %io:format("completion message was received , counter is : ~p ~n", [State#master_state.armed_SM_counter]),
  Next_Counter = State#master_state.armed_SM_counter - 1,
  MData = processSMData(State#master_state.alg,SMData, State),
  %io:format("MData = ~p~n", [MData]),
  if (State#master_state.armed_SM_counter > 1) -> %more submasters are still running the iteration
    {keep_state,State#master_state{armed_SM_counter = Next_Counter, m_supp_data = MData},{timeout,State#master_state.dims+1000,awol_sm}};
  true -> %iteration finished, now master will use algorithm specific function to process and decide how to move on
    {StrategyNew, MDataNew, Outputs,SMDataNew} = processStepData(State#master_state.alg,State#master_state{m_supp_data = MData}),
    %io:format("The strategy is : ~p ~n", [StrategyNew]),
    if (StrategyNew == proceed) -> %more iterations are required
      NextIter = State#master_state.iter + 1,
      sendGos(State#master_state.range_list,NextIter,SMDataNew),
      {keep_state, State#master_state{m_supp_data = MDataNew, iter = NextIter, armed_SM_counter = length(State#master_state.range_list), outputs = Outputs},{timeout,State#master_state.dims+1000,awol_sm}};
    true -> io:format("stopppppp, outputs are ~p ~n",[Outputs]),
      wx_object:cast(gui,{done,Outputs}), %answer to the GUI, and return all system to idle.
      killSM(State#master_state.range_list),
       %io:format("completed: ~p~n", [MDataNew]),
      {next_state, idle, State#master_state{alg = null, num_SM = 0, iter = 0, range_list = [], m_supp_data = null, armed_SM_counter = 0, outputs = null}}
    end
  end;

%%message sent from a worker to an external destination via the master
analyze(cast, {routing_internal,Dest,Msg}, State = #master_state{}) ->
  %io:format("routing_internal to : ~p msg : ~p , range list : ~p ~n", [Dest,Msg,State#master_state.range_list]),
  rerouteMsg(Dest,Msg, State#master_state.range_list), %finds the destination and sends it the message
  {keep_state,State};

%%error given from submaster, most likely a missing worker mid-iteration. algorithm is stopped and reset.
analyze(cast, error, State = #master_state{}) ->
  killSM(State#master_state.range_list),
  NewState = resetAlg(State),
  {next_state, giveOrders, NewState,{state_timeout,10000,awol_sm}};

%%a submaster timed out. algorithm is stopped and reset.
analyze(timeout,awol_sm,State) ->
  %io:format("killing sms ~n"),
  killSM(State#master_state.range_list),
  NewState = resetAlg(State),
  %io:format("rangeList is ~p~n", [NewState#master_state.range_list]),
  {next_state, giveOrders, NewState,{state_timeout,10000,awol_sm}}.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #master_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #master_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #master_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%"ping" all submasters to create the active list of them
requestSM(_,[]) -> [];
requestSM(Alg, SMList) ->
 %io:format("Smlist start:~p ~n", [hd(SMList)]),
  try
    gen_statem:call({submaster,hd(SMList)},{Alg,node()}) of
    Reply when Reply==ack -> [hd(SMList)] ++ requestSM(Alg,tl(SMList));
      %io:format("SM ~p answered wth ack~n",[hd(SMList)])
    true -> requestSM(Alg,tl(SMList))
    catch %a bad submaster reference, or an inactive one
    _:_-> requestSM(Alg,tl(SMList))
  end.

%%Main function that calculates the graph dimensions. first open up the file
checkGraphDims(FilePath) ->
  {OpenStatus,Handler} = file:open(FilePath,read), %read cmd file
  if (OpenStatus /= ok) ->
    exit(file_not_found_error);
    true ->
      Max = findMaxV(Handler,0),
      file:close(Handler),
      Max + 1
  end.

%%Read through the entire file once, to check dimensions. This is done by remembering the maximum vertex index mentioned.
findMaxV(Handler, MaxV) ->
  Line = readLine(Handler),
  if (Line == read_error) -> read_error;
  (Line == eof) -> MaxV; %at the end of the file, return the maximum index seen
  true ->
    Source = hd(Line),
    Dest = hd(tl(Line)),
    findMaxV(Handler,max(max(Source,Dest),MaxV))
  end.

%%Function for parsing a single line
readLine(Handler) ->
  Line = file:read_line(Handler),
  if (Line == eof) -> eof;
    true ->
      {ReadStatus,Raw} = Line,
      if ReadStatus /= ok -> read_error;
        true ->
          RawNoTrail = string:split(Raw,"\n",all),%remove trail character
          Str = string:split(RawNoTrail,",",all),%Split tokens
          [list_to_integer(X) || X <- Str]
      end
  end.

%%Main function for splitting the dimensions between the active members.
splitRange(ActiveList, Dims) -> splitRange(ActiveList,Dims,0, length(ActiveList)).
%assisting functions for the split with additional arguments
splitRange([], _, _,_) -> []; %if an empty list was given
splitRange([H], Dims, Offset,_) -> [{H,{Offset,Dims-1}}]; %last object
splitRange(ActiveList,Dims,Offset,Size) ->
  End = Offset+(Dims div Size),
  [{hd(ActiveList),{Offset,End - 1}}] ++ splitRange(tl(ActiveList),Dims,End,Size).


%%algorithm specific initial preperation, creating the master supp data, submaster initial data and outputs
prepAlg(mst,_State,Root) -> {ok,{Root,{inf,{null,null}}}, {0,0,os:system_time()}};
prepAlg(bellman,_State,{Root,Dest}) -> { {Root,Dest} , {false,Root,Dest,inf}, {os:system_time(),0,null,0} };
prepAlg(bfs, _State,AlgData) -> {AlgData,{false,0}, {os:system_time(),0,0}};
prepAlg(_Alg,_State, _AlgData) -> {ok,-1, demo_no_outputs}.


%%preperaion of first iteration submaster data
prepGo(mst,State) -> {Root,_} = State#master_state.m_supp_data, {search,Root};
prepGo(_,_) -> go.

%sending of the initiating messages to the active submasters (triggering their setup)
initiateSM(FilePath, RangeList, SMData) -> [ gen_statem:cast({submaster,Ref},{FilePath,Range,SMData}) || {Ref,Range} <- RangeList ].

%%This function sends out an iteration message to all submasters
sendGos(RangeList,Iter, Data) -> io:format("send iter ~p gos with ~p ~n", [Iter,Data]),
 [ gen_statem:cast({submaster,Ref},{master,Iter,Data}) || {Ref,_Range} <- RangeList ].


%%This function finds the destination of the message and sends it to the relevant submaster
rerouteMsg(_Dest, _Msg, []) -> error_bad_dest; %no submaster range found matching
rerouteMsg(Dest, Msg, [{Ref, {MinV,MaxV}} | T]) ->
if ((Dest < MinV) or (Dest > MaxV)) -> rerouteMsg(Dest,Msg,T);
                                               true -> gen_statem:cast({submaster,Ref},{routing_external,Dest,Msg}) end.


%%Algorithm specific submaster completion message processing.

%in Prim we just need to choose the lightest edge proposed, and later add that vertex.
processSMData(mst,{inf,{null,null}},State) -> State#master_state.m_supp_data;
processSMData(mst,{W,{Source,Dest}},State) ->
  {Root,{Lightest,{OldS,OldD}}} = State#master_state.m_supp_data,
  if (Lightest == inf) -> {Root,{W,{Source,Dest}}};
  (Lightest > W) ->   {Root,{W,{Source,Dest}}};
  true ->   {Root,{Lightest,{OldS,OldD}}} end;


%in bellman we first iterate until no change was made, then we reconstruct the path.
processSMData(bellman, null, State) -> State#master_state.m_supp_data; %reconstruction message, vertex not in SM
processSMData(bellman, {VID,Delta,Pi},State) -> %reconstruction message, vertex in SM
  {Root,Dest,_} = State#master_state.m_supp_data,
  {Root,Dest,{VID,Delta,Pi}};
processSMData(bellman, {Change,_,_,inf},State) -> %iteration message, no edge proposed
  %io:format("SM gave ~p~n", [{Change,inf}]),
  {CurrChange,Root,Dest,Dist} = State#master_state.m_supp_data,
  {CurrChange or Change,Root,Dest,Dist};
processSMData(bellman, {Change,_,_,DestDist},State) -> %iteration message, edge proposed
  %io:format("SM gave ~p~n", [{Change,DestDist}]),
  {CurrChange,Root,Dest,_} = State#master_state.m_supp_data,
  {CurrChange or Change,Root,Dest,DestDist};

%bfs only requires keeping track of if there were any changes, and total distance for outputs
processSMData(bfs, {Change,Delta}, State) ->
  %io:format("SM gave ~p~n", [SMData]),
  {CurrChange,TotalDelta} = State#master_state.m_supp_data,
  {CurrChange or Change,TotalDelta + Delta} ;

%%In both maxdeg algorithms, we just need to keep track of the maximum degree.
processSMData(maxddeg,SMData, State) ->
  %io:format("SM gave ~p~n", [SMData]),
  Curr = State#master_state.m_supp_data,
  if (SMData > Curr) -> SMData;
  true -> Curr end;
processSMData(maxdeg,SMData, State) ->
  %io:format("SM gave ~p~n", [SMData]),
  Curr = State#master_state.m_supp_data,
  if (SMData > Curr) -> SMData;
    true -> Curr end.


%%Algorithm specific function processStepData processes the iteration results and decides on how to move on

%in Prim, the closest vertex is added to the graph. if no vertices remain, the algorithm ends.
processStepData(mst,State) ->
  Iter = State#master_state.iter,
  {Root,{W,{_Source,Dest}}} = State#master_state.m_supp_data,
  {Weight,Iterations,Runtime} = State#master_state.outputs,
  io:format("choosing ~p~n",[W]),
  %io:format("mst step ~p with edge ~p ", [Iter,{W,Source,Dest}]),
  if ( (Iter rem 2) == 0 ) ->
    if (W == inf) ->
      {stop,State#master_state.m_supp_data,{Weight,Iterations+1,os:system_time()-Runtime},ok};
    true ->

      {proceed , {Root,{inf,{null,null}}},{Weight+W,Iterations+1,Runtime},{search,Dest}} end;
  true -> {proceed , {Root,{inf,{null,null}}},{Weight,Iterations+1,Runtime},respond} end;

%in bellman ford, if no change was made, the iterations stop. Then the path is reconstructed.
processStepData(bellman,State) ->
  {Runtime, Iterations, Path, Weight} = State#master_state.outputs,
  if(Path == null) -> %this indicates the first part of bellman ford isnt done yet
    {Change,Root,Dest,Dist} = State#master_state.m_supp_data,
    if (Change== false) -> {proceed, {Root,Dest,{null,inf,null}},{Runtime,Iterations+1,[Dest],0},{reconstruct,Dest}};
    true ->  {proceed , {false,Root,Dest,Dist},{Runtime,Iterations+1,null,0},go} end;
  true -> %we are now reconstructing the path
    {Root,Dest,{Curr,Delta,Pi}} = State#master_state.m_supp_data,
    if (Root == Pi) -> {stop,State#master_state.m_supp_data,{os:system_time()-Runtime,Iterations,[Root] ++ Path,Weight},ok};
      (Dest == Curr) ->  {proceed,State#master_state.m_supp_data,{Runtime,Iterations,[Pi] ++ Path,Delta},{reconstruct,Pi}};
    true -> {proceed,State#master_state.m_supp_data,{Runtime,Iterations,[Pi] ++ Path,Weight},{reconstruct,Pi}} end
  end;

%in bfs we only keep track of if any changes were made, and the total distance for outputs.
processStepData(bfs,State) ->
  {Runtime, Iterations,_DeltaAVG} = State#master_state.outputs,
  {Change,TotalDelta} = State#master_state.m_supp_data,
  if (Change == false) -> {stop,State#master_state.m_supp_data,{os:system_time()-Runtime,Iterations+1,TotalDelta/State#master_state.dims},ok};
  true ->  {proceed ,{false,0},{Runtime,Iterations+1,0},ok} end;

%there is no processing done in maxdeg
processStepData(maxdeg,State) -> {stop,State#master_state.m_supp_data,ok};

%maxddeg takes two iterations exactly, so there is minimal step processing.
processStepData(maxddeg,State) ->
  Iter = State#master_state.iter,
  if(Iter == 2) -> {stop,State#master_state.m_supp_data,ok};
  true -> {proceed,State#master_state.m_supp_data,ok} end.

%%sending out messages to the SMs, to reset themselves.
killSM([]) ->   io:format("killing sms all done ~n");
killSM([{Ref,_} | T]) ->   io:format("killing sm ~p~n", [Ref] ),
  gen_statem:cast({submaster,Ref},exit) , killSM(T).


%performing the necessary subset of operations that are in the idle state, to restrat the algorithm.
resetAlg(State) -> SMList = breakdownRangeList(State#master_state.range_list),
  ActiveList = requestSM(State#master_state.alg,SMList),
  io:format("activelist ~p~n",[ActiveList]),
  RangeList = splitRange(ActiveList,State#master_state.dims), %splits and sends to each SM his range.
  NumSM = length(RangeList),
  {SMData,MData,Outputs} = prepAlg(State#master_state.alg,State,State#master_state.algdata),
  initiateSM(State#master_state.filepath,RangeList,SMData),
  State#master_state{range_list = RangeList, num_SM = NumSM, m_supp_data = MData, armed_SM_counter = 0, outputs = Outputs} .

%reverting the rangelist to an SMlist.
breakdownRangeList([]) -> [];
breakdownRangeList([{Ref,_} | T]) -> [Ref] ++ breakdownRangeList(T).

