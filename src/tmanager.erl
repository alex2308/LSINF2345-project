%%%-------------------------------------------------------------------
%%% @author gillonb
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2018 11:02
%%%-------------------------------------------------------------------
-module(tmanager).
-author("gillonb").

%% API
-export([update/3,snapshot_read/2,connect/1]).

%% Function run at load of module
-on_load(init/0).
connect(Pid)->
  Pid ! {connect,self()},
  receive
    {successfulConnect,T,PartitionList} -> io:format("Succesfully connected to DB at time ~p ~n",[T]);
    _ -> io:format("not able to connect to datastore ~n")
  end.
update(Key,Value,PartitionList) ->
  %% Find in which datastore the Key is stored (HASH)
  %V = hash(Key),
  Size = length(PartitionList),
  Rem = (Key rem Size) - 1, %pour simplifier j'ai juste fait le reste de la division par 2
  ToSend = lists:nth(Rem,PartitionList),
  ToSend ! {update,Key,Value},
  io:format("Sending the following request ~p to ~p ~n ",[{update,Key,Value},ToSend]),
  ok
.


snapshot_read(Key,PartitionList) ->
  T = os:timestamp(),
  %% Returns the list of Values read at that time snapshot
  Size = length(PartitionList),
  Rem = (Key rem Size) - 1, %pour simplifier j'ai juste fait le reste de la division par 2
  ToSend = lists:nth(Rem,PartitionList),
  ToSend ! {read,T,Key},
  io:format("Sending the following request ~p to ~p ~n ",[{read,T,Key},ToSend]),
  ok
.


hash(Key) ->
  D = crypto:hash(md4,Key),
  binary_to_integer(D)
.


init() ->
  io:format("Initialization of the transaction manager ~n")

.
