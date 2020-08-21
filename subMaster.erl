%%%-------------------------------------------------------------------
%%% @author Or
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. Aug 2020 1:21 PM
%%%-------------------------------------------------------------------
-module(subMaster).
-author("Or").

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).


%% states
-export([idle/3,setup/3,giveOrders/3,analyze/3]).


-define(SERVER, ?MODULE).

-record(subMaster_state, {alg, num_workers, idle_workers, sm_supp_data,master_node}). %supp data is O(1) data for alg purposes.

name() -> submaster.
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link() ->
  gen_statem:start_link({local, name()}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([]) ->
  dets:open_file(graphDB, []), %FIXME - check for valid arguments
  dets: delete_all_objects(graphDB),
  {ok, idle, #subMaster_state{alg = null, num_workers = 0,idle_workers = 0,sm_supp_data = null, master_node = null}}. %FIXME - supp data may not need inst.

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

%%Assume alg is from the valid set
idle({call,From}, {Alg,MNode}, State = #subMaster_state{}) ->
     io:format("Submaster start prepare, Alg:~p ~n", [Alg]),
     io:format("From:~p ~n", [From]),

  {next_state, setup, State#subMaster_state{alg = Alg, master_node = MNode},{reply, From, ack}}. %update the algorithm before moving on.



setup(cast, {FilePath,Range = {MinV,MaxV},Data}, State = #subMaster_state{}) ->
  NumWorkers = MaxV - MinV + 1,
  io:format("NumWorkers:~p ~n", [NumWorkers]),
  {WorkerData,SMData} = prepAlg(State#subMaster_state.alg,Data,State),
  readGraph(FilePath,Range,State#subMaster_state.alg,WorkerData), %FIXME - perhaps spawn monitors and receive here
  io:format("The graph was read ~n", []),
  io:format("SMData was generated: ~p  ~n", [SMData]),
  gen_statem:cast({master,State#subMaster_state.master_node},{ok}), %FIXME need to do it generic
  {next_state, giveOrders, State#subMaster_state{num_workers = NumWorkers, idle_workers = NumWorkers, sm_supp_data = SMData}}.


%%Message from master, of incoming rerouted message from external machine to local worker.
giveOrders(cast, {routing_external,Dest,Msg}, State = #subMaster_state{}) ->
io:format("giveOrders : external routing to ~p , Msg is : ~p  ~n", [Dest,Msg]),
  passMsg(external,Dest,Msg,State#subMaster_state.master_node),
  {keep_state,State};

giveOrders(cast, {master, Iter, Data}, State = #subMaster_state{}) -> %Go
  io:format("Give orders, iter: ~p , data : ~p   ~n", [Iter, Data]),
  {WorkerData,SMData} = handleIter(State#subMaster_state.alg,Data,State),
  sendOrders(Iter,WorkerData),
  {next_state, analyze, State#subMaster_state{idle_workers = 0, sm_supp_data = SMData}};
giveOrders({call,From}, {exit}, State = #subMaster_state{}) ->
  killWorkers(State#subMaster_state.num_workers),
  {next_state, idle, State#subMaster_state{alg = null, num_workers = 0, idle_workers = 0, sm_supp_data = null},
  {reply, From, finished_successfully}}.


%%Message from internal machine worker, to reroute a message to vertex in external machine.
analyze(cast, {routing_internal,Dest,Msg}, State = #subMaster_state{}) ->
io:format("analyze : internal routing to ~p , Msg is : ~p  ~n", [Dest,Msg]),

  passMsg(internal,Dest,Msg,State#subMaster_state.master_node),
  {keep_state,State};

%%Message from master, of incoming rerouted message from external machine to internal worker.
analyze(cast, {routing_external,Dest,Msg}, State = #subMaster_state{}) ->
io:format("analyze : external routing to ~p , Msg is : ~p  ~n", [Dest,Msg]),
  passMsg(external,Dest,Msg,State#subMaster_state.master_node),
  {keep_state,State};

analyze(cast, {completion,VID,Status,Data}, State = #subMaster_state{}) ->

 if (Status == ok) ->
   SMData = handleData(State#subMaster_state.alg,State,VID,Data), %algorithm specific supplementary data.
   Idle = State#subMaster_state.idle_workers,
   Total = State#subMaster_state.num_workers,
   io:format("complition was received from worker: ~p : ~p. ~p left~n", [VID,Data,Idle]),
   if (Idle < (Total-1)) ->
     {keep_state,State#subMaster_state{sm_supp_data = SMData , idle_workers = Idle+1}};
   true ->
      io:format("completed:~n", []),
     gen_statem:cast({master,State#subMaster_state.master_node},{completion,SMData}), %FIXME - maybe cast is possible, by creating server reference.Jonathan- changed from call to cast
     {next_state, giveOrders ,State#subMaster_state{sm_supp_data = SMData , idle_workers = Idle+1}} end;
 true -> handleBadWorker(VID) end.





%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(_EventType, _EventContent, _StateName, State = #subMaster_state{}) ->
  NextStateName = the_next_state_name,
  {next_state, NextStateName, State}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #subMaster_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #subMaster_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


readGraph(FilePath, {MinV,MaxV},Alg,WorkerData) ->
  {OpenStatus,Handler} = file:open(FilePath,read), %read cmd file
  if (OpenStatus /= ok) ->
    exit(file_not_found_error); %FIXME - exit?
  true ->
    readRange(MaxV,Handler,MinV,[],Alg,WorkerData),
    file:close(Handler)
  end.

readRange(MaxV,Handler, VIndex, Neighbours,Alg,WorkerData) ->
  Line = readLine(Handler),
  if (Line == read_error) -> read_error;
  (Line == eof) ->
    %FIXME - spawn isolated
    PID = spawn(worker,workerInit,[Alg, VIndex,WorkerData]),
    %PID ! doit ,
    io:format("new worker : ~p with pid: ~p and neighbours ~p ~n", [VIndex,PID, Neighbours]),
    dets:insert_new(graphDB,{VIndex,{PID,Neighbours}}),
    ok;
  true ->
    CurrIndex = hd(Line),
    Dest = hd(tl(Line)),
    Weight = hd(tl(tl(Line))),
    if (CurrIndex > VIndex) ->
      PID = spawn(worker,workerInit,[Alg, VIndex,WorkerData]),
      dets:insert_new(graphDB,{VIndex,{PID,Neighbours}}),
    io:format("new worker : ~p with pid: ~p ~n", [VIndex,PID]),
      spawnIsolated(VIndex,min(CurrIndex,MaxV+1), Alg,WorkerData),
      if(CurrIndex > MaxV) -> ok; %FIXME - return value
      true ->
        readRange(MaxV,Handler,CurrIndex,[{Dest,Weight}],Alg,WorkerData)
      end;
    true ->
      if(CurrIndex == VIndex) ->
        readRange(MaxV, Handler, VIndex, Neighbours ++ [{Dest, Weight}], Alg,WorkerData );
      true -> %This means VIndex == MinV, and CurrIndex<MinIndex
         readRange(MaxV,Handler,VIndex,Neighbours, Alg,WorkerData) end
    end
  end.

spawnIsolated(Start, End, Alg,WorkerData) ->
  if (Start < (End - 1)) ->
    Index = Start+1,
    PID = spawn(worker,workerInit,[Alg, Index,WorkerData]),
    io:format("new isolated worker : ~p with pid: ~p ~n", [Index,PID]),

    dets:insert_new(graphDB,{Index,{PID,[]}}),
    spawnIsolated(Index,End, Alg,WorkerData);
  true -> ok end.

readLine(Handler) ->
  Line = file:read_line(Handler),
  if (Line == eof) -> eof;
  true ->
    {ReadStatus,Raw} = Line,
    if ReadStatus /= ok -> read_error;
    true ->
      RawNoTrail = string:split(Raw,"\n",all),
      Str = string:split(RawNoTrail,",",all),
      [list_to_integer(X) || X <- Str]
    end
  end.


sendOrders(Iter, Data) ->
  Key = dets:first(graphDB),
  io:format("sendOrders, the key is: ~p ~n", [Key]),
  A= dets:lookup(graphDB,Key),
  io:format(" ~p  was read from dets ~n", [A]),
  [{_,{PID,_L}}]=A,
  io:format(" pid is: ~p  ~n", [is_pid(PID)]),
  PID ! {Iter,Data},
  io:format(" just see if it falls here ~n", []),
  sendOrders(Iter,Data,Key).

sendOrders(Iter,Data,Key) ->
  NKey = dets:next(graphDB,Key),
  Obj = dets:lookup(graphDB,NKey),
  if (Obj == '$end_of_table') -> ok;
  Obj == [] -> ok;
  true ->
    [{_Num,{PID,_A}}] = Obj,
    io:format(" going to send ~p , to ~p at iter ~p  ~n", [Data , PID, Iter]),
    PID ! {Iter,Data},
    sendOrders(Iter,Data,NKey)
  end.

%TODO - endgame
killWorkers(StateNumWorkers) -> ok.


handleData(mst,State,_VID,ok) -> State#subMaster_state.sm_supp_data;
handleData(mst,State,VID,{NVID,W}) ->
  {Lightest,{Source,Dest}} = State#subMaster_state.sm_supp_data,
  if (Lightest == inf) -> {W,{NVID,VID}};
  (Lightest > W) ->   {W,{NVID,VID}};
  true -> {Lightest, {Source,Dest}} end;


handleData(bellman,State,VID,{Change,Delta,_}) ->
  {CurrChange,Root,Dest,DestDist} = State#subMaster_state.sm_supp_data,
  Result = (CurrChange or Change),
  io:format("result is ~p~n",[Result]),
  if(VID == Dest) -> {Result,Root,Dest,Delta };
    true -> {Result,Root,Dest,DestDist } end;

handleData(bfs,State,_VID,{Change,_,_}) ->
  Result = (State#subMaster_state.sm_supp_data or Change),
  io:format("result is ~p~n",[Result]), Result;

handleData(maxddeg,State,_VID, Data) ->
  Curr = State#subMaster_state.sm_supp_data,
  if (Data =/= null) ->
    if (Data>Curr) -> Data;
    true -> Curr end;
  true -> Curr end;
handleData(maxdeg,State,_VID, Data) ->
  Curr = State#subMaster_state.sm_supp_data,
  if (Data>Curr) -> Data;
    true -> Curr end.

passMsg(external,Dest, Msg,_MNode) ->
  [{_Num,{PID,_A}}] = dets:lookup(graphDB,Dest),
  PID ! Msg;
passMsg(internal,Dest, Msg,MNode) ->
  gen_statem:cast({master,MNode},
    {routing_internal,Dest,Msg}).%FIXME - generic


%TODO - endgame
handleBadWorker(VID) -> ok. %FIXME - perhaps unnecessary.


prepAlg( mst, _Data, State) -> {ok,ok};
prepAlg( bellman, {Root,Dest}, State) -> {Root,{false,Root,Dest,inf }};
prepAlg( bfs, Data, State) -> {Data,false};
prepAlg( Alg, Data, State) -> {ok,-1}.



handleIter(mst,WorkerData,State) -> {WorkerData,{inf,{null,null}}};
handleIter(bellman,Data,State) -> {_,Root,Dest,DestDist} = State#subMaster_state.sm_supp_data,
  {go,{false,Root,Dest,DestDist}};
handleIter(bfs,Data,State) -> {go,false};
handleIter(maxddeg, Data, State) -> {go,State#subMaster_state.sm_supp_data};
handleIter(maxdeg,Data,State) -> {go,State#subMaster_state.sm_supp_data}.