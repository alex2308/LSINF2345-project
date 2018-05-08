%%%-------------------------------------------------------------------
%%% @author Bastien Gillon, Alexandre Carlier
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2018 11:02
%%%-------------------------------------------------------------------
-module(datastore).
-author("Bastien Gillon, Alexandre Carlier").

%% API export
-export([start_shell/1,start/2,core/2,connectionHandler/1]).

%% Timer in ms for snapshotread
-define(TIMERMS,10).


%% Delay in ms. usefull for gc
-define(DELAY,500).


%%
%% Function to run from shell command with following arguments:
%%    - number of data stores you want to start
%%    - name you want to give to your store connector. will be registered in this atom
%%
start_shell(ListArgs) ->
  if length(ListArgs) /= 2 -> io:format("Number of args not OK~n"),init:stop();
    true ->
      N = list_to_integer(lists:nth(1,ListArgs)),
      Name = list_to_atom(lists:nth(2,ListArgs)),
      start(N,Name)
  end
.


%%
%% Function that generates N stores and register the connection handler with ConnectName
%%  Arg:  - N: integer representing the number of store you want to open
%%        - ConnectName: atom where the store connector or connection handler will be stored
%%
start(N,ConnectName) ->
  List = generateNstores(N,"datastore_"),
  S = spawn(datastore,connectionHandler,[List]),
  _R = register(ConnectName,S),
  io:format("# Datastore ~p (Pid ~p) is running on node ~p #~n",[ConnectName,S,node()])
.

%%
%% Function that runs in a node an that can connect tmanagers to all the stores or close all the stores
%%  Args: -StoresList: List of stores Pid
%%
%%
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
%% Function that spawns and registers N store with the name Name_0 to Name_N-1
%%  Args: - N: integer representing the number of data stores you want to launchin a local node
%%        - Name: an atom that will be used to register the new store.
%%
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

%%
%% Function that has the main behavior of a data store
%% Args:  - Store: a dictionnary containing all keys with their associated values
%%        - BufferOld: a list of tuples {Time,Key,ResponsePid,Uid,Index} that
%%            keeps track of snapshots that needs to wait
%%
core(Store,BufferOld) ->
  Buffer = tryEmpty(BufferOld,Store), %% First check if buffer can be emptied
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
      ResponsePid ! {ok,update,Uid}, % respond update ok to tmanager
      core(NewStore,Buffer);
    {read,Time,Key,ResponsePid,Uid,Index} ->
      P = dict:find(Key,Store),
      %io:format("Received the following request from Transaction manager: ~p ~n",[{read,Time,Key}]),
      case P of
        {ok,History} -> %
          %io:format("The values for ~p are ~p ~n",[Key,History]),
          T = os:timestamp(),
          {Value,_UnusedTime} = lists:nth(1,History),
          if T >= Time -> % data is valid for snapshot
              ResponsePid ! {ok,read,Uid,Index,Value},
              core(Store,Buffer);
            true -> % data is not yet valid for snapshot
              BufferElem = {Time,Key,ResponsePid,Uid,Index},
              io:format("WAIT ~n"),
              _TimerRef = timerClean(self(),ceil(timer:now_diff(Time,T)/1000)), % set timer to not forget to empty buffer when data valid
              core(Store,lists:append(Buffer,[BufferElem])) % add element to buffer
          end;
        error -> % Key not avaible: should not happen
          io:format("Should not happen (Key not avaible) ~n"),
          core(Store,Buffer) %% TODO:
      end;
    {gc,ResponsePid,Uid} ->
      NStoreList = clean_store(dict:to_list(Store)), %% try to clean the store
      NStore = dict:from_list(NStoreList),
      ResponsePid ! {ok,gc,Uid}, %% send ok message to tmanager
      %%io:format("Do gc~n")
      core(NStore,Buffer);
    {clean} -> %% Message from timer to clean buffer => restart function
      io:format("Clean Buffer"),
      core(Store,Buffer);
    {exit} -> %% stop data store
      io:format("Datastore terminated ~n")
  end
.

%%
%% Function used by the core function to try to emtpy the Buffer of waiting snapshot_reads
%%
%% Returns: Buffer with less or equal size as before
%%
tryEmpty(Buffer,Store) ->
  case Buffer of
    [] ->
      [];
    [H|T] ->
      {Time,Key,ResponsePid,Uid,Index} = H,
      CTime = os:timestamp(),
      if Time < CTime -> % valid!
          P = dict:find(Key,Store),
          case P of
            {ok,[{Value,_UnusedTime},_Tail]} ->
              ResponsePid ! {ok,read,Uid,Index,Value}
          end,
          [tryEmpty(T,Store)];
        true -> % invalid
          [H|tryEmpty(T,Store)]
      end
  end
.

%%
%% Function used by the core function to send a clean message after some defined time (Milliseconds)
%%  which is the time needed before a snapshot can be read. sending the clean command will force the
%%  core function to try to empty the buffer of waiting snapshot_reads
%%
timerClean(DataCorePid,Milliseconds) ->
  case timer:send_after(Milliseconds,DataCorePid,{clean}) of
    {ok,Tref} -> Tref;
    {error,Reason} -> io:format("Error send_after: ~p ~n",[Reason]),none
  end
.



%%
%% Function called by core function when garbage collection should be done
%%  and return a new store with less or equal values
%%
clean_store(Store) ->
  case Store of
    [] ->
      [];
    [{Key,HistList}|T] ->
      % only try to remove values when more than 1 value avaible
      if (length(HistList) >= 1) ->
          [{Key,clean_hist_list(HistList,1)}|clean_store(T)];
        true ->
          [{Key,HistList}|clean_store(T)]
      end
  end
.

%% Remove all unaccessible Values of the value history list
%%
clean_hist_list(HistList,N) ->
  case HistList of
    [] ->
      [];
    [{Value,TimeStamp}|T] ->
      if (N == 1) ->
          [{Value,TimeStamp}|clean_hist_list(T,0)];
        true ->
          {MS,S,Ms} = TimeStamp,
          Tn = {MS,S,Ms+(?DELAY)},
          Ti = os:timestamp(),
          if (Ti > Tn) ->
              [];
            true ->
              [clean_hist_list(T,0)]
          end
      end
  end
.