%%%-------------------------------------------------------------------
%%% @author Or
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Aug 2020 8:43 PM
%%%-------------------------------------------------------------------
-module(worker).
-author("Or").

%% API
-export([workerInit/3]).


workerInit(maxdeg,VID,_Data) -> %max degree algorithm init
  %io:format("Worker: ~p was initiated ~n", [VID]),
  maxDegListen(VID);
workerInit(maxddeg,VID,_Data) -> %max double degree algorithm init
  %io:format("Worker: ~p was initiated (ddeg) ~n", [VID]),
  maxDDegListen(VID,null,0,0);
workerInit(bfs,VID,Root) -> % Breadth-first search algorithm init
  %io:format("Worker: ~p was initiated (bfs) ~n", [VID]),
  if(Root == VID) ->
    self() ! {0,{discover,null,0}},
    bfsListen(VID,null,inf,null);
  true -> bfsListen(VID,null,inf,null) end;
workerInit(bellman,VID,Root) -> %Bellman ford (shoretest path) algorithm init
  %io:format("Worker: ~p was initiated (bellman)  with root ~p~n", [VID,Root]),
  if(Root == VID) ->
    %io:format("he is root"),
    self() ! {0,{relax,null,0}},
    bellListen(VID,null,inf,null);
    true -> bellListen(VID,null,inf,null) end;
workerInit(mst,VID,_Data) -> %minimum spanning tree algorithm init
    mstListen(VID,null,false,null,inf);
workerInit(_Unkown,_,_Data) ->
  exit(unkown_algorithm).

%%Max degree algorithm - each worker count all his neighbours and send the number to the submaster
maxDegListen(VID) -> %waiting for orders from the submaster
%io:format("Worker: ~p is going to receive block ~n", [VID]),
			receive
                    _M -> %io:format("Worker: ~p received a message ~n", [VID]),
			  gen_statem:cast(submaster,{completion,VID,checkDeg(VID)}) %send completion to the submaster
      after 60000 -> exit(sm_timeout)
                  end.



checkDeg(VID) -> [{_,{_PID, Neighbours}}]= ets:lookup(graphDB,VID),
  %io:format("Neighbours for ~p is ~p ~n", [VID,Neighbours])
  length(Neighbours). %count the number of neighbours


%%Max double degree algorithm - each worker send a message to his neighbours and wait for messages, the worker count all the undirected edges and send the number to the submaster
maxDDegListen(VID,Neighbours,Iter,DDeg) ->
  %io:format("Worker: ~p is going to receive block ~n", [VID]),
  receive %waiting for orders from the submaster
    {SMIter, {neighbour,NVID}} when (SMIter < Iter) -> %receive only messages from the previous iteration
      if(Neighbours == null) ->   [{_,{_PID, NewNeighbours}}] = ets:lookup(graphDB,VID); %read the neighbours list from the ets only if it is the first time
        true-> NewNeighbours = Neighbours end,

        IsNeighbour = checkNeighbour(NVID,NewNeighbours), 
      %io:format("Neighbours for ~p is ~p ~n", [VID,Neighbours]),
      %io:format("are ~p and ~p neighbours? ~p~n",[VID,NVID,IsNeighbour]),
      if (IsNeighbour == true) -> maxDDegListen(VID,NewNeighbours,Iter,DDeg+1);
        true -> maxDDegListen(VID,NewNeighbours,Iter,DDeg) end;

    {SMIter,go} ->
      if(Neighbours == null) ->   [{_,{_PID, NewNeighbours}}] = ets:lookup(graphDB,VID); %read the neighbours list from the ets only if it is the first time
        true-> NewNeighbours = Neighbours end,

      if (SMIter == 1) -> %iteration 1, send messages to the neighbors
        [sendNeighbour(NVID,{1,{neighbour,VID}}) || {NVID,_} <- NewNeighbours], %FIXME
  	%io:format("Worker: ~p is going to send completion message ~n", [VID]),
        gen_statem:cast(submaster,{completion,VID,null}),
        maxDDegListen(VID,NewNeighbours,1,DDeg);
      (SMIter == 2) -> maxDDegListen(VID,NewNeighbours,2,DDeg);  %%iteration 2 receive messages from neighbours
      true ->
          maxDDegListen(VID,Neighbours,Iter,DDeg)
      end
  after 0 -> if (Iter == 2) ->  gen_statem:cast(submaster,{completion,VID,DDeg}); %after the mailbox is empty, send completion to submaster
             true -> maxDDegListen(VID,Neighbours,Iter,DDeg) end
  end.

sendNeighbour(NVID,Msg) ->
  Obj = ets:lookup(graphDB,NVID),
  if (Obj == []) -> 
%io:format("going to send routing ~p  to ~p ~n", [Msg ,NVID]),
gen_statem:cast(submaster,{routing_internal,NVID, Msg});
  true -> [{_,{PID, _}}] = Obj,
%io:format("going to send ~p  to ~p ~n", [Msg , PID]),
    PID ! Msg end.



checkNeighbour(_NVID, []) -> false;
checkNeighbour(NVID,[{NVID,_} | _T]) -> true;
checkNeighbour(NVID,Neighbours) -> checkNeighbour(NVID,tl(Neighbours)).





%%BFS - Worker goes over mailbox to look for {discover, NVID, Dist}. If not yet discovered, updates memory and sends discover messages to neighbors.
bfsListen(VID, Neighbours, Delta, Pi) ->
  %io:format("Worker: ~p is going to receive block ~n", [VID]),
  receive
    {SMIter,go} ->
      if(Neighbours == null) ->   [{_,{_PID, NewNeighbours}}] = ets:lookup(graphDB,VID); %read the neighbours list from the ets only if it is the first time
        true-> NewNeighbours = Neighbours end,

      {Change,NewDelta,NewPi} = bfsCheckMail(VID,NewNeighbours, false, SMIter, Delta, Pi),
      gen_statem:cast(submaster,{completion,VID,{Change,NewDelta,NewPi}}),
      bfsListen(VID, NewNeighbours, NewDelta,NewPi)
  after 60000 -> exit(sm_timeout)
  end.

bfsCheckMail(VID,Neighbours, Change,Iter, Delta, Pi) ->
  receive
    {WIter,{discover,NVID,Dist}} when WIter<Iter -> %receive only messages from the previous iteration
      if (Delta == inf) -> %if it is the first time that the is discovered , send message to his neighbours
        [sendNeighbour(Neighbour,{Iter,{discover,VID,Dist+1}}) || {Neighbour,_} <- Neighbours], %side effects
        bfsCheckMail(VID,Neighbours,true,Iter,Dist,NVID);
      true -> bfsCheckMail(VID, Neighbours, Change,Iter,Delta, Pi) end %if the workers was already been discovered, ignore the message
    after 0 -> {Change,Delta,Pi}
  end.

%%Bellman Ford Worker goes over mailbox to look for {relax, NVID, Dist}. If an effective relax occurs, sends relax messages to all neighbors. Algorithm ends when no successful relaxes occur. In the “reconstruct phase”, each iteration the master requests the vertex prior to a specific V, which is the only responder.

bellListen(VID, Neighbours,  Delta, Pi) ->
  %io:format("Worker: ~p is going to receive block ~n", [VID]),
  receive
    {SMIter,go} ->
      if(Neighbours == null) ->   [{_,{_PID, NewNeighbours}}] = ets:lookup(graphDB,VID); %read the neighbours list from the ets only if it is the first time
        true-> NewNeighbours = Neighbours end,

      {Change,NewDelta,NewPi} = bellCheckMail(VID,NewNeighbours, SMIter, false,Delta, Pi), %if not all the vertices were discovered
      gen_statem:cast(submaster,{completion,VID,{Change,NewDelta,NewPi}}),
      bellListen(VID, NewNeighbours, NewDelta,NewPi);

    {_SMIter,{reconstruct,VID}} -> %if  all the vertices were discovered, we want the reconstruct the path
      io:format("I am found ~p ~n",[VID]),
      gen_statem:cast(submaster,{completion,VID, {Delta,Pi}}),
      bellListen(VID, Neighbours,  Delta, Pi);

    {_SMIter,{reconstruct,_}} -> gen_statem:cast(submaster,{completion,VID, ok}),
      bellListen(VID, Neighbours,  Delta, Pi)


%%    {SMIter,{request_path,Dest,Root}} ->
%%      if(VID == Dest) ->
%%        sendNeighbour(Pi,{concat_path,[VID],Root}),
%%        gen_statem:cast(submaster,{completion,VID,{false,Delta,Pi}});
%%      (VID == Root) -> bellListen(VID, Neighbours, SMIter, Delta,Pi);
%%      true ->
%%        gen_statem:cast(submaster,{completion,VID,{false,Delta,Pi}}),
%%        bellListen(VID, Neighbours, SMIter, Delta,Pi)
%%      end;
%%    {concat_path,Path,Root} ->
%%      if(VID == Root) -> gen_statem:cast(submaster,{completion,VID,(Path++[VID])});
%%      true -> sendNeighbour(Pi,{concat_path,[VID],Root}) end
  after 60000 -> exit(sm_timeout)
  end.

bellCheckMail(VID,Neighbours, Iter, Change, Delta, Pi) ->
  %io:format("Worker ~p is checking mail ~n", [VID]),
  receive
    {WIter,{relax,NVID,Dist}} when WIter<Iter -> %receive only messages from the previous iteration
      %io:format("Worker with inf dist ~p got relax from ~p with dist ~p~n",[VID,NVID,Dist]),
      if (Delta == inf) -> %if it is the first time that the is discovered , send message to his neighbours
        [sendNeighbour(Neighbour,{Iter,{relax,VID,Dist+W}}) || {Neighbour,W} <- Neighbours], %side effects
        bellCheckMail(VID,Neighbours,Iter,true,Dist,NVID);
      (Delta > Dist) -> %if it is not the first time that the is discovered , but a new short path to this vertex was discovered send message to his neighbours
        %io:format("Worker with high dist ~p got relax from ~p with dist ~p~n",[VID,NVID,Dist]),
        [sendNeighbour(Neighbour,{Iter,{relax,VID,Dist+W}}) || {Neighbour,W} <- Neighbours], %side effects
        bellCheckMail(VID,Neighbours,Iter,true,Dist,NVID);
      true -> bellCheckMail(VID, Neighbours, Iter, Change,Delta, Pi) end
  after 0 -> {Change,Delta,Pi}
  end.


%%MST -Search step: The master declares a newly added vertex. That vertex sends {annex,_} messages to all neighbors. All other vertices remove that vertex from the neighbor list. Respond step: each non-annexed vertex reports the lightest edge connecting some annexed vertex to it. 

mstListen(VID, Neighbours, false,Pi,Delta) ->
  receive
    {SMIter, {search, VID}} ->
      if(Neighbours == null) ->
        [{_,{_PID, NewNeighbours}}] = ets:lookup(graphDB,VID); %read the neighbours list from the ets only if it is the first time
        true-> NewNeighbours = Neighbours end,
      %io:format("Vertex ~p has neighbours ~p to contact~n",[VID,NewNeighbours]),
      [sendNeighbour(Neighbour,{SMIter,{annex,VID,W}}) || {Neighbour,W} <- NewNeighbours], %side effects
      gen_statem:cast(submaster,{completion,VID,ok}),
      mstListen(VID,[],true,Pi,Delta);
    {_SMIter, {search, OVID}} ->
      if(Neighbours == null) ->
        [{_,{_PID, EtsNeighbours}}] = ets:lookup(graphDB,VID); %read the neighbours list from the ets only if it is the first time
        true-> EtsNeighbours = Neighbours end,
      NewNeighbours = removeOVID(EtsNeighbours,OVID),

      gen_statem:cast(submaster,{completion,VID,ok}),
      mstListen(VID,NewNeighbours,false,Pi,Delta);
    {SMIter, respond} ->
      {NewPi,NewDelta} = mstCheckMail(VID,Neighbours, SMIter, Pi,Delta),
      gen_statem:cast(submaster,{completion,VID, {NewPi,NewDelta}}),
      mstListen(VID,Neighbours,false,NewPi,NewDelta)
  after 60000 -> exit(sm_timeout)
  end;

mstListen(VID, Neighbours, true,Pi,Delta) ->
  if(Neighbours == null) ->   [{_,{_PID, EtsNeighbours}}] = ets:lookup(graphDB,VID); %read the neighbours list from the ets only if it is the first time
    true-> EtsNeighbours = Neighbours end,

  receive
    {_SMIter, {search, OVID}} ->
      NewNeighbours = removeOVID(EtsNeighbours,OVID),

      %[sendNeighbour(Neighbour,{SMIter,{annex,VID,W}}) || {Neighbour,W} <- NewNeighbours], %side effects
      gen_statem:cast(submaster,{completion,VID,ok}),
      mstListen(VID,NewNeighbours,true,Pi,Delta);
    {_SMIter, respond} ->
      gen_statem:cast(submaster,{completion,VID,ok}),
      mstListen(VID,Neighbours,true,Pi,Delta)

  after 60000 -> exit(sm_timeout)
  end.


mstCheckMail(VID, Neighbours, Iter, ChosenPi,ChosenW) ->
  %io:format("Worker ~p is checking mail ~n", [VID]),
  receive
    {WIter,{annex,NVID,W}} when WIter<Iter -> %receive only messages from the previous iteration
      %io:format("Worker with inf dist ~p got relax from ~p with dist ~p~n",[VID,NVID,Dist]),
      if (ChosenW == inf) ->
        mstCheckMail(VID, Neighbours, Iter, NVID,W);
        (W < ChosenW) ->
          %io:format("Worker with high dist ~p got relax from ~p with dist ~p~n",[VID,NVID,Dist]),
          mstCheckMail(VID, Neighbours, Iter, NVID,W);
        true -> mstCheckMail(VID, Neighbours, Iter, ChosenPi,ChosenW) end
  after 0 -> {ChosenPi,ChosenW}
  end.



removeOVID([], _OVID) -> [];
removeOVID([{OVID,_} | T], OVID) -> T;
removeOVID([H | T], OVID) -> [H] ++ removeOVID(T,OVID).