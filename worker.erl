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
-export([workerInit/2]).


workerInit(maxdeg,VID) ->
  io:format("Worker: ~p was initiated ~n", [VID]),
  maxDegListen(VID);
workerInit(maxddeg,VID) ->
  io:format("Worker: ~p was initiated ~n", [VID]),
  [{_,{_PID, Neighbours}}] = dets:lookup(graphDB,VID),
  maxDDegListen(VID,Neighbours,0,0);
workerInit(_Unkown,_) -> exit(unkown_algorithm).

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
      IsNeighbour = lists:member(NVID,Neighbours),
      if (IsNeighbour == true) -> maxDDegListen(VID,Neighbours,Iter,DDeg+1);
        true -> maxDDegListen(VID,Neighbours,Iter,DDeg) end;

    {SMIter,go} ->
      if (SMIter == 1) ->
        [sendNeighbour(NVID,{1,{neighbour,VID}}) || NVID <- Neighbours],
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
  if (Obj == []) -> gen_statem:cast(submaster,{routing_internal,NVID, Msg});
  true -> [{_,{PID, _}}] = Obj,
    PID ! Msg end.

