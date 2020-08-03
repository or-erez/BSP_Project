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
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1, format_status/2, handle_event/4, terminate/3,
  code_change/4, callback_mode/0]).

%state functions
-export([idle/3,giveOrders/3,analyze/3]).

-define(SERVER, ?MODULE).

-record(master_state, {alg, num_SM, iter, range_list,m_supp_data, armed_SM_counter}).

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
  {ok, state_name, #master_state{alg = null, num_SM = 0, iter = 0, range_list = [], m_supp_data = null, armed_SM_counter = 0}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  handle_event_function.

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

idle({call,From}, {Alg,SMList,FilePath,AlgData}, State = #master_state{}) -> %FIXME maybe a cast is also applicable.
  ActiveList = requestSM(Alg,SMList),
  V = checkGraphDims(FilePath),
  {NumSM,RangeList} = splitRange(ActiveList,V), %splits and sends to each SM his range.
  {SMData,MData} = prepAlg(State,AlgData),
  initiateSM(FilePath,RangeList,SMData),
  {next_state,setup,State#master_state{range_list = RangeList, m_supp_data = MData, num_SM = NumSM},[reply, From, ack]}.

giveOrders(cast, {Response}, State = #master_state{}) -> %FIXME - this state is a bug! we do double cast for call, and timeout == endless loop.
  Next_Counter = State#master_state.armed_SM_counter + 1,
  if (State#master_state.armed_SM_counter < (State#master_state.num_SM-1)) ->
    {keep_state, State#master_state{armed_SM_counter = Next_Counter}};
  true -> sendGos(State), {next_state, analyze, State#master_state{armed_SM_counter = Next_Counter}} end.


analyze(cast, {routing_local,Dest,Msg}, State = #master_state{}) ->
  rerouteMsg(Dest,Msg, State),
  {keep_state,State};

analyze(cast, {completion, SMData}, State = #master_state{}) -> %FIXME - need timeout event as well.
  Next_Counter = State#master_state.armed_SM_counter - 1,
  MData = processSMData(SMData, State),
  if (State#master_state.armed_SM_counter > 1) ->
    {keep_state,State#master_state{armed_SM_counter = Next_Counter, m_supp_data = MData}};
  true ->
    {Strategy, MData} = processStepData(State),
    if (Strategy == proceed) ->
      NextIter = State#master_state.iter + 1,
      {keep_state, State#master_state{m_supp_data = MData, iter = NextIter}};
    true -> removeSM(State),
      {next_state, idle, State#master_state{alg = null, num_SM = 0, iter = 0, range_list = [], m_supp_data = null, armed_SM_counter = 0}}
    end
  end.


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


requestSM(Alg, SMList) ->
  erlang:error(not_implemented).

checkGraphDims(FilePath) ->
  {OpenStatus,Handler} = file:open(FilePath,read), %read cmd file
  if (OpenStatus /= ok) ->
    exit(file_not_found_error); %FIXME - exit?
    true ->
      Max = findMaxV(Handler,0),
      file:close(Handler),
      Max
  end.

findMaxV(Handler, MaxV) ->
  Line = readLine(Handler),
  if (Line == read_error) -> read_error;
  (Line == eof) -> MaxV;
  true ->
    Source = hd(Line),
    Dest = hd(tl(Line)),
    findMaxV(Handler,max(max(Source,Dest),MaxV))
  end.

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

splitRange(ActiveList, V) ->
  erlang:error(not_implemented).

prepAlg(State, AlgData) ->
  erlang:error(not_implemented).

initiateSM(FilePath, RangeList, SMData) ->
  erlang:error(not_implemented).

sendGos(State) ->
  erlang:error(not_implemented).

rerouteMsg(Dest, Msg, State) ->
  erlang:error(not_implemented).

processSMData(SMData, State) ->
  erlang:error(not_implemented).

processStepData(State) ->
  erlang:error(not_implemented).

removeSM(State) ->
  erlang:error(not_implemented).

