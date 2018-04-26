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
-export([startshell/1,start/2,core/2,connectionHandler/1]).

-define(TIMERMS,10).


%%
%% -Storage: a dictonary with key and as value a list of tuples. The tuples contain
%%  {Value of key,timestamp}. This list is oreder from new to old timestamps
%%
%% -Main function 'core' is hidden from user. Only interaction happens through 'update' and
%%  'snapshot_read'

startshell(ListArgs) ->
  if length(ListArgs) /= 2 -> io:format("Number of args not OK~n"),exit;
    true ->
      N = list_to_integer(lists:nth(1,ListArgs)),
      Name = list_to_atom(lists:nth(2,ListArgs)),
      start(N,Name)
  end
.

%% Start N datastores and return the running node that handles the connections to it
%%
start(N,ConnectName) ->
  List = generateNstores(N,"datastore_"),
  S = spawn(datastore,connectionHandler,[List]),
  _R = register(ConnectName,S),
  io:format("# Datastore ~p (Pid ~p) is running on node ~p #~n",[S,ConnectName,node()])
.

connectionHandler (StoresList) ->
  receive
    {connect,Pid} ->
      Pid ! {ok,StoresList},
      connectionHandler(StoresList);
    {exit} ->
      lists:map(fun (X) -> X ! {exit} end,StoresList)
  end
.

%%
generateNstores(N,Name) ->
  if N==0 -> [];
    true ->
      Rname = lists:append(Name,integer_to_list(N)),
      AtomRname = list_to_atom(Rname),
      Dict = dict:new(),
      SPid = spawn(datastore,core,[Dict,[]]),
      register(AtomRname,SPid),
      [SPid|generateNstores(N-1,Name)]
  end
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
          io:format("Should not happen (Key not avaible) ~n"),
          core(Store,Buffer) %% TODO:
      end;
    {gc,ResponsePid,Uid} ->
      ResponsePid ! {ok,gc,Uid},
      io:format("GC not implemented yet on store~n"),
      core(Store,Buffer);
    {clean} -> %% Message from timer to clean buffer => restart function
      io:format("Clean Buffer"),
      core(Store,Buffer);
    {exit} ->
      io:format("Datastore terminated ~n")
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

