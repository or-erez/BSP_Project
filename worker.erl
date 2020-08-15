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

maxDegListen(VID) -> 
io:format("Worker: ~p is going to receive block ~n", [VID]),
			receive
                    _M -> io:format("Worker: ~p received a message ~n", [VID]),
			  gen_statem:cast(submaster,{completion,VID,ok,checkMaxDeg(VID)})
                  end,
maxDegListen(VID).



checkMaxDeg(VID) -> [{_,{_PID, Neighbours}}]= dets:lookup(graphDB,VID), length(Neighbours).