%%%-------------------------------------------------------------------
%%% @author Bastien Gillon
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2018 11:02
%%%-------------------------------------------------------------------
-module(datastore).
-author("Bastien Gillon").

%% API export
-export([start/0,core/2]).

-define(TIMERMS,10).


%%
%% -Storage: a dictonary with key and as value a list of tuples. The tuples contain
%%  {Value of key,timestamp}. This list is oreder from new to old timestamps
%%
%% -Main function 'core' is hidden from user. Only interaction happens through 'update' and
%%  'snapshot_read'


%% Starts 2 node running the core function and registering the 2 nodes as 'data1' and 'data2'
%%
start() ->
  Dict1 = dict:new(),
  Dict2 = dict:new(),
  Data1 = spawn(datastore,core,[Dict1,[]]),
  register(data1,Data1),
  Data2 = spawn(datastore,core,[Dict2,[]]),
  register(data2,Data2)
.

%% MAIN function
%%
%%
core(Store,BufferOld) ->
  %% First check if buffer can be emptied
  Buffer = tryEmpty(BufferOld,Store),
  receive
    {update,Key,Value,ResponsePid,Uid} ->
      P = dict:find(Key,Store),
      %io:format("Received the following request from Transaction manager: ~p ~n",[{update,Key,Value}]),
      case P of
        {ok,OldHistory} -> % Value already present
          NewHistory = lists:append([{Value,os:timestamp()}],OldHistory), % put the new tuple in front a OldHistory to create new History
          NewStore = dict:store(Key,NewHistory,Store); % Erase old History by overwritting
        error -> % new Value to be stored
          NewStore = dict:store(Key,[{Value,os:timestamp()}],Store)
      end,
      ResponsePid ! {ok,update,Uid},
      core(NewStore,Buffer);
    {read,Time,Key,ResponsePid,Uid,Index} ->
      P = dict:find(Key,Store),
      %io:format("Received the following request from Transaction manager: ~p ~n",[{read,Time,Key}]),
      case P of
        {ok,History} -> %
          %io:format("The values for ~p are ~p ~n",[Key,History]),
          T = os:timestamp(),
          {Value,_UnusedTime} = lists:nth(1,History),
          if T >= Time ->
              %io:format("OK data valid~n"),
              ResponsePid ! {ok,read,Uid,Index,Value},
              core(Store,Buffer);
            true ->
              %io:format("Data not yet valid~n"),
              BufferElem = {Time,Key,ResponsePid,Uid,Index},
              _TimerRef = timerClean(self(),timer:now_diff(Time,T)/1000), %% TODO: maybe to many timers
              core(Store,lists:append(Buffer,[BufferElem]))
          end;
        error -> % should not happen
          io:format("Should not happen ~n")
      end;
    {gc,ResponsePid,Uid} ->
      ResponsePid ! {ok,gc,Uid},
      io:format("GC not implemented yet on store~n"),
      core(Store,Buffer);
    {clean} -> %% Message from timer to clean buffer => restart function
      io:format("Clean Buffer"),
      core(Store,Buffer);
    {stop} ->
      io:format("Stop datastore ~n")
  end
.

tryEmpty(Buffer,Store) ->
  case Buffer of
    [] ->
      [];
    [H|T] ->
      {Time,Key,ResponsePid,Uid,Index} = H,
      CTime = os:timestamp(),
      if Time < CTime ->
          P = dict:find(Key,Store),
          case P of
            {ok,[{Value,_UnusedTime},_Tail]} ->
              ResponsePid ! {ok,read,Uid,Index,Value}
          end;
        true -> %
          NList = lists:append([H],T),
          NList
      end,
      tryEmpty(T,Store)
  end
.


timerClean(DataCorePid,Milliseconds) ->
  case timer:send_after(Milliseconds,DataCorePid,{clean}) of
    {ok,Tref} -> Tref;
    {error,_Reason} -> io:format("Error send_after~n"),none
  end
.

