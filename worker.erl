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
  %io:format("Worker: ~p was initiated ~n", [VID]),
  maxDegListen(VID);
workerInit(maxddeg,VID,_Data) ->
  %io:format("Worker: ~p was initiated (ddeg) ~n", [VID]),
  maxDDegListen(VID,null,0,0);
workerInit(bfs,VID,Root) ->
  %io:format("Worker: ~p was initiated (bfs) ~n", [VID]),
  if(Root == VID) ->
    self() ! {0,{discover,null,0}},
    bfsListen(VID,null,inf,null);
  true -> bfsListen(VID,null,inf,null) end;
workerInit(bellman,VID,Root) ->
  %io:format("Worker: ~p was initiated (bellman)  with root ~p~n", [VID,Root]),
  if(Root == VID) ->
    %io:format("he is root"),
    self() ! {0,{relax,null,0}},
    bellListen(VID,null,inf,null);
    true -> bellListen(VID,null,inf,null) end;
workerInit(_Unkown,_,_Data) ->
  exit(unkown_algorithm).

maxDegListen(VID) -> 
%io:format("Worker: ~p is going to receive block ~n", [VID]),
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
      if(Neighbours == null) ->   [{_,{_PID, NewNeighbours}}] = dets:lookup(graphDB,VID);
        true-> NewNeighbours = Neighbours end,

        IsNeighbour = checkNeighbour(NVID,NewNeighbours),
      %io:format("Neighbours for ~p is ~p ~n", [VID,Neighbours]),
      %io:format("are ~p and ~p neighbours? ~p~n",[VID,NVID,IsNeighbour]),
      if (IsNeighbour == true) -> maxDDegListen(VID,NewNeighbours,Iter,DDeg+1);
        true -> maxDDegListen(VID,NewNeighbours,Iter,DDeg) end;

    {SMIter,go} ->
      if(Neighbours == null) ->   [{_,{_PID, NewNeighbours}}] = dets:lookup(graphDB,VID);
        true-> NewNeighbours = Neighbours end,

      if (SMIter == 1) ->
        [sendNeighbour(NVID,{1,{neighbour,VID}}) || {NVID,_} <- NewNeighbours], %FIXME
  	%io:format("Worker: ~p is going to send completion message ~n", [VID]),
        gen_statem:cast(submaster,{completion,VID,ok,null}),
        maxDDegListen(VID,NewNeighbours,1,DDeg);
      (SMIter == 2) -> maxDDegListen(VID,NewNeighbours,2,DDeg);
      true ->
          maxDDegListen(VID,Neighbours,Iter,DDeg)
      end
  after 0 -> if (Iter == 2) ->  gen_statem:cast(submaster,{completion,VID,ok,DDeg});
             true -> maxDDegListen(VID,Neighbours,Iter,DDeg) end
  end.

sendNeighbour(NVID,Msg) ->
  Obj = dets:lookup(graphDB,NVID),  
  if (Obj == []) -> 
%io:format("going to send routing ~p  to ~p ~n", [Msg ,NVID]),
gen_statem:cast(submaster,{routing_internal,NVID, Msg});
  true -> [{_,{PID, _}}] = Obj,
%io:format("going to send ~p  to ~p ~n", [Msg , PID]),
    PID ! Msg end.



checkNeighbour(_NVID, []) -> false;
checkNeighbour(NVID,[{NVID,_} | _T]) -> true;
checkNeighbour(NVID,Neighbours) -> checkNeighbour(NVID,tl(Neighbours)).

bfsListen(VID, Neighbours, Delta, Pi) ->
  %io:format("Worker: ~p is going to receive block ~n", [VID]),
  receive
    {SMIter,go} ->
      if(Neighbours == null) ->   [{_,{_PID, NewNeighbours}}] = dets:lookup(graphDB,VID);
        true-> NewNeighbours = Neighbours end,

      {Change,NewDelta,NewPi} = bfsCheckMail(VID,NewNeighbours, SMIter, false,Delta, Pi),
      gen_statem:cast(submaster,{completion,VID,ok,{Change,NewDelta,NewPi}}),
      bfsListen(VID, NewNeighbours, NewDelta,NewPi)
  end.

bfsCheckMail(VID,Neighbours, Change,Iter, Delta, Pi) ->
  receive
    {WIter,{discover,NVID,Dist}} when WIter<Iter ->
      if (Delta == inf) ->
        [sendNeighbour(Neighbour,{Iter,{discover,VID,Dist+1}}) || {Neighbour,_} <- Neighbours], %side effects
        bfsCheckMail(VID,Neighbours,Iter,true,Dist,NVID);
      true -> bfsCheckMail(VID, Neighbours, Iter, Change,Delta, Pi) end
    after 0 -> {Change,Delta,Pi}
  end.

bellListen(VID, Neighbours,  Delta, Pi) ->
  %io:format("Worker: ~p is going to receive block ~n", [VID]),
  receive
    {SMIter,go} ->
      if(Neighbours == null) ->   [{_,{_PID, NewNeighbours}}] = dets:lookup(graphDB,VID);
        true-> NewNeighbours = Neighbours end,

      {Change,NewDelta,NewPi} = bellCheckMail(VID,NewNeighbours, SMIter, false,Delta, Pi),
      gen_statem:cast(submaster,{completion,VID,ok,{Change,NewDelta,NewPi}}),
      bellListen(VID, NewNeighbours, NewDelta,NewPi)
%%    {SMIter,{request_path,Dest,Root}} ->
%%      if(VID == Dest) ->
%%        sendNeighbour(Pi,{concat_path,[VID],Root}),
%%        gen_statem:cast(submaster,{completion,VID,ok,{false,Delta,Pi}});
%%      (VID == Root) -> bellListen(VID, Neighbours, SMIter, Delta,Pi);
%%      true ->
%%        gen_statem:cast(submaster,{completion,VID,ok,{false,Delta,Pi}}),
%%        bellListen(VID, Neighbours, SMIter, Delta,Pi)
%%      end;
%%    {concat_path,Path,Root} ->
%%      if(VID == Root) -> gen_statem:cast(submaster,{completion,VID,ok,(Path++[VID])});
%%      true -> sendNeighbour(Pi,{concat_path,[VID],Root}) end
  end.

bellCheckMail(VID,Neighbours, Iter, Change, Delta, Pi) ->
  %io:format("Worker ~p is checking mail ~n", [VID]),
  receive
    {WIter,{relax,NVID,Dist}} when WIter<Iter ->
      %io:format("Worker with inf dist ~p got relax from ~p with dist ~p~n",[VID,NVID,Dist]),
      if (Delta == inf) ->
        [sendNeighbour(Neighbour,{Iter,{relax,VID,Dist+W}}) || {Neighbour,W} <- Neighbours], %side effects
        bellCheckMail(VID,Neighbours,Iter,true,Dist,NVID);
      (Delta > Dist) ->
        %io:format("Worker with high dist ~p got relax from ~p with dist ~p~n",[VID,NVID,Dist]),
        [sendNeighbour(Neighbour,{Iter,{relax,VID,Dist+W}}) || {Neighbour,W} <- Neighbours], %side effects
        bellCheckMail(VID,Neighbours,Iter,true,Dist,NVID);
      true -> bellCheckMail(VID, Neighbours, Iter, Change,Delta, Pi) end
  after 0 -> {Change,Delta,Pi}
  end.
