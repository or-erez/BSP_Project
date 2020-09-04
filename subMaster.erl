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
  ets:new(graphDB, [named_table,set]), %setting up the database
  {ok, idle, #subMaster_state{alg = null, num_workers = 0,idle_workers = 0,sm_supp_data = null, master_node = null}}.

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

%%The initial state. It is assumed that Alg is from a valid set.
idle({call,From}, {Alg,MNode}, State = #subMaster_state{}) ->
     %io:format("Submaster start prepare, Alg:~p ~n", [Alg]),
     %io:format("From:~p ~n", [From]),
     ets:delete_all_objects(graphDB), %upon starting a new algorithm, database is cleared
  {next_state, setup, State#subMaster_state{alg = Alg, master_node = MNode},[{reply, From, ack},{state_timeout, 10000, master_dead}]}. %update the algorithm before moving on.


%%Setup state, after receiving details from master the sub-graph is read. This is a cast (not a call) so submasters can work concurrently.
setup(cast, {FilePath,Range = {MinV,MaxV},Data}, State = #subMaster_state{}) ->
  NumWorkers = MaxV - MinV + 1,%it is assumed the graph is split into homogenous blocks
  %io:format("NumWorkers:~p ~n", [NumWorkers]),
  {WorkerData,SMData} = prepAlg(State#subMaster_state.alg,Data,State), %algorithm specific preparations create supp data and first worker data
  readGraph(FilePath,Range,State#subMaster_state.alg,WorkerData),
  %io:format("The graph was read ~n", []),
  %io:format("SMData was generated: ~p  ~n", [SMData]),
  gen_statem:cast({master,State#subMaster_state.master_node},{ok}), %acknowledgment that graph has been read
  {next_state, giveOrders, State#subMaster_state{num_workers = NumWorkers, idle_workers = NumWorkers, sm_supp_data = SMData},{state_timeout, 10000, master_dead}};

%%Timeout from master in this state
setup(state_timeout,master_dead,State) ->
  killWorkers(),
  {next_state, idle, State#subMaster_state{alg = null, num_workers = 0, idle_workers = 0, sm_supp_data = null}}.



%%Iteration begins, master gives iteration number and data
giveOrders(cast, {master, Iter, Data}, State = #subMaster_state{}) -> %Go
  %io:format("Give orders, iter: ~p , data : ~p   ~n", [Iter, Data]),
  {WorkerData,SMData} = handleIter(State#subMaster_state.alg,Data,State), %algorithm specific pre-iteration handling
  sendOrders(Iter,WorkerData), %distribute orders to all workers
  {next_state, analyze, State#subMaster_state{idle_workers = 0, sm_supp_data = SMData},{state_timeout,State#subMaster_state.num_workers+10000,awol_worker}};

%%Message from master, of incoming rerouted message from external machine to local worker.
giveOrders(cast, {routing_external,Dest,Msg}, State = #subMaster_state{}) ->
%io:format("giveOrders : external routing to ~p , Msg is : ~p  ~n", [Dest,Msg]),
  passMsg(external,Dest,Msg,State#subMaster_state.master_node),%message is passed to worker
  {keep_state,State};

%%End of algorithm and return to idle (can be due to errors found by master as well)
giveOrders(cast, exit, State = #subMaster_state{}) ->
  %io:format("exiting ~n~n~n~n~n~n~n~n~n~n~n"),
  killWorkers(),
  {next_state, idle, State#subMaster_state{alg = null, num_workers = 0, idle_workers = 0, sm_supp_data = null}};

%%master timeout in this state
giveOrders(state_timeout,master_dead,State) ->
  killWorkers(),
  {next_state, idle, State#subMaster_state{alg = null, num_workers = 0, idle_workers = 0, sm_supp_data = null}}.



%%Iteration state, waiting for all workers to finish
analyze(cast, {completion,VID,Data}, State = #subMaster_state{}) ->
   SMData = handleData(State#subMaster_state.alg,State,VID,Data), %algorithm specific supplementary data.
   Idle = State#subMaster_state.idle_workers,
   Total = State#subMaster_state.num_workers,
   %io:format("completion was received from worker: ~p : ~p. ~p left~n", [VID,Data,Idle]),
   if (Idle < (Total-1)) ->
     {keep_state,State#subMaster_state{sm_supp_data = SMData , idle_workers = Idle+1}};
   true -> %iteration done
      %io:format("completed:~n", []),
     gen_statem:cast({master,State#subMaster_state.master_node},{completion,SMData}),
     {next_state, giveOrders ,State#subMaster_state{sm_supp_data = SMData , idle_workers = Idle+1}} end;

%%Message from internal machine worker, to reroute a message to vertex in external machine.
analyze(cast, {routing_internal,Dest,Msg}, State = #subMaster_state{}) ->
%io:format("analyze : internal routing to ~p , Msg is : ~p  ~n", [Dest,Msg]),
  passMsg(internal,Dest,Msg,State#subMaster_state.master_node), %pass message to master for rerouting
  {keep_state,State};

%%Message from master, of incoming rerouted message from external machine to internal worker.
analyze(cast, {routing_external,Dest,Msg}, State = #subMaster_state{}) ->
%io:format("analyze : external routing to ~p , Msg is : ~p  ~n", [Dest,Msg]),
  passMsg(external,Dest,Msg,State#subMaster_state.master_node), %pass message to worker
  {keep_state,State};

%%timeout from worker in this state
analyze(state_timeout,awol_worker,State) ->
  killWorkers(),
  gen_statem:cast({master,State#subMaster_state.master_node},error),
  {next_state, idle, State#subMaster_state{alg = null, num_workers = 0, idle_workers = 0, sm_supp_data = null}};

%%For some reason, the master decides to abort the algorithm
analyze(cast, exit, State = #subMaster_state{}) ->
  %io:format("exiting ~n~n~n~n~n~n~n~n~n~n~n"),
  killWorkers(),
  {next_state, idle, State#subMaster_state{alg = null, num_workers = 0, idle_workers = 0, sm_supp_data = null}}.


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

%%Reading the graph main function
readGraph(FilePath, {MinV,MaxV},Alg,WorkerData) ->
  {OpenStatus,Handler} = file:open(FilePath,read), %read cmd file
  if (OpenStatus /= ok) ->
    exit(file_not_found_error);
  true ->
    readRange(MaxV,Handler,MinV,[],Alg,WorkerData),
    file:close(Handler)
  end.

%%Read the relevant vertex range from the opened file
readRange(MaxV,Handler, VIndex, Neighbours,Alg,WorkerData) ->
  Line = readLine(Handler),
  if (Line == read_error) -> read_error;
  (Line == eof) -> %file is finished
    PID = spawn(worker,workerInit,[Alg, VIndex,WorkerData]), %previous worker spawned
    %io:format("new worker : ~p with pid: ~p and neighbours ~p ~n", [VIndex,PID, Neighbours]),
    ets:insert_new(graphDB,{VIndex,{PID,Neighbours}}),
    ok;
  true -> %need to decipher line
    CurrIndex = hd(Line),
    Dest = hd(tl(Line)),
    Weight = hd(tl(tl(Line))),
    if (CurrIndex > VIndex) -> % last worker has no more neighbors
      PID = spawn(worker,workerInit,[Alg, VIndex,WorkerData]), %create worker
      ets:insert_new(graphDB,{VIndex,{PID,Neighbours}}),
    %io:format("new worker : ~p with pid: ~p ~n", [VIndex,PID]),
      spawnIsolated(VIndex,min(CurrIndex,MaxV+1), Alg,WorkerData), %handles all isolated vertices between the previous worker and the next one
      if(CurrIndex > MaxV) -> ok; %finish reading
      true ->
        readRange(MaxV,Handler,CurrIndex,[{Dest,Weight}],Alg,WorkerData) %move on
      end;
    true -> %not done with current index
      if(CurrIndex == VIndex) -> %found a neighbor, move on
        readRange(MaxV, Handler, VIndex, Neighbours ++ [{Dest, Weight}], Alg,WorkerData );
      true -> %This means VIndex == MinV, and CurrIndex<MinIndex, meaning we haven't reached the required range yet
         readRange(MaxV,Handler,VIndex,Neighbours, Alg,WorkerData) end
    end
  end.

%%Spawns workers representing isolated vertices with indexes [Start+1,End-1]
spawnIsolated(Start, End, Alg,WorkerData) ->
  if (Start < (End - 1)) ->
    Index = Start+1,
    PID = spawn(worker,workerInit,[Alg, Index,WorkerData]),
    %io:format("new isolated worker : ~p with pid: ~p ~n", [Index,PID]),

    ets:insert_new(graphDB,{Index,{PID,[]}}),
    spawnIsolated(Index,End, Alg,WorkerData);
  true -> ok end.

%parse a single line
readLine(Handler) ->
  Line = file:read_line(Handler),
  if (Line == eof) -> eof; %end of file
  true ->
    {ReadStatus,Raw} = Line,
    if ReadStatus /= ok -> read_error;
    true ->
      RawNoTrail = string:split(Raw,"\n",all), %removing trailing character
      Str = string:split(RawNoTrail,",",all),
      [list_to_integer(X) || X <- Str]
    end
  end.


%Main function to start iteration with all workers. iterates over the database to reach all workers, sending same message to all.
sendOrders(Iter, Data) ->
  Key = ets:first(graphDB),
  %io:format("sendOrders, the key is: ~p ~n", [Key]),
  A= ets:lookup(graphDB,Key),
  %io:format(" ~p  was read from ets ~n", [A]),
  [{_,{PID,_L}}]=A,
  %io:format(" pid is: ~p  ~n", [is_pid(PID)]),
  PID ! {Iter,Data},
  %io:format(" just see if it falls here ~n", []),
  sendOrders(Iter,Data,Key).

%keeps iterating using ets:next() until end of table
sendOrders(Iter,Data,Key) ->
  NKey = ets:next(graphDB,Key),
  Obj = ets:lookup(graphDB,NKey),
  if (Obj == '$end_of_table') -> ok;
  Obj == [] -> ok;
  true ->
    [{_Num,{PID,_A}}] = Obj,
    %io:format(" going to send ~p , to ~p at iter ~p  ~n", [Data , PID, Iter]),
    PID ! {Iter,Data},
    sendOrders(Iter,Data,NKey)
  end.

%send kill message to all workers, same way as sendOrders
killWorkers() -> Top = ets:first(graphDB),
  if (Top =/= '$end_of_table') ->
    [{First,{PID,_}}] = ets:lookup(graphDB,Top),
    exit(PID,kill),
    killWorkers(First);
  true -> ok end.
killWorkers(Curr) -> Next = ets:next(graphDB,Curr),
  if (Next =/= '$end_of_table') ->
    [{NewCurr,{PID,_}}] = ets:lookup(graphDB,Next),
    exit(PID,kill),
    killWorkers(NewCurr);
    true -> ok end.


%%Algorithm specific function handleData handles worker responses, creating the supplementary data for the submaster

handleData(mst,State,_VID,ok) -> State#subMaster_state.sm_supp_data;
handleData(mst,State,VID,{NVID,W}) ->
  {Lightest,{Source,Dest}} = State#subMaster_state.sm_supp_data,
  if (Lightest == inf) -> {W,{NVID,VID}};
  (Lightest > W) ->   {W,{NVID,VID}};
  true -> {Lightest, {Source,Dest}} end;


handleData(bellman,State,VID,{Change,Delta,_}) -> %first part of bellman - update until no change
  {CurrChange,Root,Dest,DestDist} = State#subMaster_state.sm_supp_data,
  Result = (CurrChange or Change),
  %io:format("result is ~p~n",[Result]),
  if(VID == Dest) -> {Result,Root,Dest,Delta };
    true -> {Result,Root,Dest,DestDist } end;
handleData(bellman,_State,VID,{Delta,Pi}) -> {VID,Delta,Pi}; %second part of bellman - reconstruction of path
handleData(bellman,State,_VID,ok) -> State#subMaster_state.sm_supp_data;

handleData(bfs,State,_VID,{Change,Delta,_}) -> %keeps track of the change and total distance
  {CurrChange, TotalDelta} = State#subMaster_state.sm_supp_data,
  Result = (CurrChange or Change),
  if(Delta == inf) -> {Result,TotalDelta};
  true -> {Result,TotalDelta+Delta} end;

handleData(maxddeg,State,_VID, Data) -> %keeps track of maximum
  Curr = State#subMaster_state.sm_supp_data,
  if (Data =/= null) ->
    if (Data>Curr) -> Data;
    true -> Curr end;
  true -> Curr end;
handleData(maxdeg,State,_VID, Data) -> %keeps track of maximum
  Curr = State#subMaster_state.sm_supp_data,
  if (Data>Curr) -> Data;
    true -> Curr end.


%%passes message from external source to worker
passMsg(external,Dest, Msg,_MNode) ->
  %io:format("result is ~p~n",[Dest]),
  [{_Num,{PID,_A}}] = ets:lookup(graphDB,Dest),
  PID ! Msg;

%%passes message from worker to external destination
passMsg(internal,Dest, Msg,MNode) ->
  gen_statem:cast({master,MNode},
    {routing_internal,Dest,Msg}).


%%Algorithm specific preparation, that creates the initial supplementary data and worker data
prepAlg( mst, _Data, _State) -> {ok,ok};
prepAlg( bellman, {Root,Dest}, _State) -> {Root,{false,Root,Dest,inf }};
prepAlg( bfs, Data, _State) -> {Data,false};
prepAlg( _Alg, _Data, _State) -> {ok,-1}.



%%Algorithm specific preparation of each iteration, that creates the supplementary data and worker data
handleIter(mst,WorkerData,_State) -> {WorkerData,{inf,{null,null}}};
handleIter(bellman, go,State) -> {_,Root,Dest,DestDist} = State#subMaster_state.sm_supp_data,
  {go,{false,Root,Dest,DestDist}};
handleIter(bellman,Data,_State) -> {Data,null};
handleIter(bfs,_Data,_State) -> {go,{false,0}};
handleIter(maxddeg, _Data, State) -> {go,State#subMaster_state.sm_supp_data};
handleIter(maxdeg,_Data,State) -> {go,State#subMaster_state.sm_supp_data}.

