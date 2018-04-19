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
-export([start/1,loop/1,connect/1]).

%% Function run at load of module
-on_load(init/0).


%% Implemenation
%%
%% -Storage: a dictonary with key and as value a list of tuples. The tuples contain
%%  {Value of key,timestamp}. This list is oreder from new to old timestamps
%%
%% -Main function 'loop' is hidden from user. Only interaction happens through 'update' and
%%  'snapshot_read'
%%

%update(Key,Value) ->
%  loop ! {update,Key,Value},
%  ok
%.


%snapshot_read(Snapshot_time,Key) ->
  %% Returns the list of Values read at that time snapshot
%  loop ! {read,Snapshot_time,Key}
%.
init(PartitionList) ->
  start(PartitionList),
  ok.
start(PartitionList) ->
  Instantiation = fun(X) ->
    io:format("Creating partition ~p ~n",[X]),
    Dict = dict:new(),
    Data = spawn(datastore,loop,[Dict]),
    register(X,Data)
  end,
  Connector = spawn(datastore,connect,[PartitionList]),
  io:format("Pid to connect to DB is ~p ~n",[Connector]),
  lists:foreach(Instantiation,PartitionList),
  io:format("The following partitions have been created ~p ~n",[PartitionList]).

connect(PartitionList) ->
  io:format("Listening for incoming connection to the DB ~n"),
  receive
    {connect,TMid} -> io:format("TM with id: ~p is connecting to DB ~n",[TMid]),T = os:timestamp(), TMid ! {successfulConnect,T,PartitionList};
    _ -> io:format("Wrong connection pattern sent to DB ~n")
  end,
  connect(PartitionList).
%% Handle update/read etc here. hidden from user.
loop(Store) ->
%  if length(ReadBuffer) /= 0 -> % check if we can reduce
%    io:format()
%  end,
  receive
    {update,Key,Value} ->
      P = dict:find(Key,Store),
      io:format("Received the following request from Transaction manager: ~p ~n",[{update,Key,Value}]),
      case P of
        {ok,OldHistory} -> % Value already present
          NewHistory = lists:append([{Value,os:timestamp()}],OldHistory), % put the new tuple in front a OldHistory to create new History
          NewStore = dict:store(Key,NewHistory,Store), % Erase old History by overwritting
          loop(NewStore);
        error -> % new Value to be stored
          NewStore = dict:store(Key,[{Value,os:timestamp()}],Store),
          loop(NewStore)
      end;
    {read,Time,Key} -> % Foreach key in Keys return only valid data!
      P = dict:find(Key,Store),
      io:format("Received the following request from Transaction manager: ~p ~n",[{read,Time,Key}]),
      case P of
        {ok,History} -> %
          io:format("The values for ~p are ~p ~n",[Key,History]),
          T = os:timestamp(),
        if T > Time ->
            io:format("then");
          true ->
            io:format("else")
        end
      end
    end
.

%% Function launched at import module
