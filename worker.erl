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


workerInit(_Alg,VID,_SM) ->
io:format("Worker: ~p was initiated ~n", [VID]),
 maxDegListen(VID).

maxDegListen(VID) -> receive
                    {_,_} -> gen_statem:cast(submaster,{completion,VID,ok,checkMaxDeg(VID)})
                  end.



checkMaxDeg(VID) -> [{_,Neighbours}] = dets:lookup(graphDB,VID), length(Neighbours).