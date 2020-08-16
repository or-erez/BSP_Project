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


workerInit(maxdeg,VID,_Data) ->
  io:format("Worker: ~p was initiated ~n", [VID]),
  maxDegListen(VID);
workerInit(maxddeg,VID,_Data) ->
  io:format("Worker: ~p was initiated (ddeg) ~n", [VID]),
  [{_,{_PID, Neighbours}}] = dets:lookup(graphDB,VID),
  maxDDegListen(VID,Neighbours,0,0);
workerInit(bfs,VID,Root) ->
  io:format("Worker: ~p was initiated (bfs) ~n", [VID]),
  [{_,{_PID, Neighbours}}] = dets:lookup(graphDB,VID),
  if(Root == VID) ->
    self() ! {0,{discover,null,0}},
    bfsListen(VID,Neighbours,0,inf,null);
  true -> bfsListen(VID,Neighbours,0,inf,null) end;
workerInit(_Unkown,_,_Data) ->
  exit(unkown_algorithm).

maxDegListen(VID) -> 
io:format("Worker: ~p is going to receive block ~n", [VID]),
			receive
                    _M -> io:format("Worker: ~p received a message ~n", [VID]),
			  gen_statem:cast(submaster,{completion,VID,ok,checkDeg(VID)})
                  end.



checkDeg(VID) -> [{_,{_PID, Neighbours}}]= dets:lookup(graphDB,VID),
  io:format("Neighbours for ~p is ~p ~n", [VID,Neighbours]),length(Neighbours).

maxDDegListen(VID,Neighbours,Iter,DDeg) ->
  %io:format("Worker: ~p is going to receive block ~n", [VID]),
  receive
    {SMIter, {neighbour,NVID}} when (SMIter < Iter) ->
      IsNeighbour = checkNeighbour(NVID,Neighbours),
      io:format("Neighbours for ~p is ~p ~n", [VID,Neighbours]),
      io:format("are ~p and ~p neighbours? ~p~n",[VID,NVID,IsNeighbour]),
      if (IsNeighbour == true) -> maxDDegListen(VID,Neighbours,Iter,DDeg+1);
        true -> maxDDegListen(VID,Neighbours,Iter,DDeg) end;

    {SMIter,go} ->
      if (SMIter == 1) ->
        [sendNeighbour(NVID,{1,{neighbour,VID}}) || {NVID,_} <- Neighbours], %FIXME
  	io:format("Worker: ~p is going to send completion message ~n", [VID]),
        gen_statem:cast(submaster,{completion,VID,ok,null}),
        maxDDegListen(VID,Neighbours,1,DDeg);
      (SMIter == 2) -> maxDDegListen(VID,Neighbours,2,DDeg);
      true ->
          maxDDegListen(VID,Neighbours,Iter,DDeg)
      end
  after 0 -> if (Iter == 2) ->  gen_statem:cast(submaster,{completion,VID,ok,DDeg});
             true -> maxDDegListen(VID,Neighbours,Iter,DDeg) end
  end.

sendNeighbour(NVID,Msg) ->
  Obj = dets:lookup(graphDB,NVID),  
  if (Obj == []) -> 
io:format("going to send routing ~p  to ~p ~n", [Msg ,NVID]),
gen_statem:cast(submaster,{routing_internal,NVID, Msg});
  true -> [{_,{PID, _}}] = Obj,
io:format("going to send ~p  to ~p ~n", [Msg , PID]),
    PID ! Msg end.



checkNeighbour(_NVID, []) -> false;
checkNeighbour(NVID,[{NVID,_} | _T]) -> true;
checkNeighbour(NVID,Neighbours) -> checkNeighbour(NVID,tl(Neighbours)).

bfsListen(VID, Neighbours, Iter, Delta, Pi) ->
  io:format("Worker: ~p is going to receive block ~n", [VID]),
  receive
    {SMIter,go} -> {Change,NewDelta,NewPi} = bfsCheckMail(VID,Neighbours, Iter, false,Delta, Pi),
      gen_statem:cast(submaster,{completion,VID,ok,{Change,NewDelta,NewPi}}),
      bfsListen(VID, Neighbours, SMIter, NewDelta,NewPi)
  end.

bfsCheckMail(VID,Neighbours, Iter, Change, Delta, Pi) ->
  receive
    {WIter,{discover,NVID,Dist}} when WIter<Iter ->
      if (Delta == inf) ->
        [sendNeighbour(NVID,{Iter,{discover,VID,Dist+1}}) || {NVID,_} <- Neighbours], %side effects
        bfsCheckMail(VID,Neighbours,Iter,true,Dist,NVID);
      true -> bfsCheckMail(VID, Neighbours, Iter, Change,Delta, Pi) end
    after 0 -> {Change,Delta,Pi}
  end.