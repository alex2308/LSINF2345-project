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
-export([update/3,snapshot_read/2,connect/1,hash/1]).

%% Function run at load of module
-on_load(init/0).
connect(Pid)->
  Pid ! {connect,self()},
  receive
    {successfulConnect,T,PartitionList} -> io:format("Succesfully connected to DB at time ~p ~n",[T]);
    _ -> io:format("not able to connect to datastore ~n")
  end.
%% Key has to be an atom in order to have the hash function
update(Key,Value,PartitionList) ->
  %% Find in which datastore the Key is stored (HASH)
  Value_to_hash = erlang:atom_to_list(Key),
  Hash= crypto:hash(md5,Value_to_hash),
  Size = length(PartitionList),
  HashList = erlang:binary_to_list(Hash),
  Rem = (lists:last(HashList) rem Size) - 1, %pour simplifier j'ai juste fait le reste de la division par 2
  ToSend = lists:nth(Rem,PartitionList),
  ToSend ! {update,Key,Value},
  io:format("Sending the following request ~p to ~p ~n ",[{update,Key,Value},ToSend]),
  ok
.


snapshot_read(Keys,PartitionList) ->
  T = os:timestamp(),
  snapshot_read(Keys,T,PartitionList).
  %% Returns the list of Values read at that time snapshot
snapshot_read(Keys,Time,PartitionList) ->
  case Keys of
    [] -> ok;
    [Head|Tail] ->
                  Value_to_hash = erlang:atom_to_list(Head),
                  Hash= crypto:hash(md5,Value_to_hash),
                  Size = length(PartitionList),
                  HashList = erlang:binary_to_list(Hash),
                  Rem = (lists:last(HashList) rem Size) - 1, %pour simplifier j'ai juste fait le reste de la division par 2
                  ToSend = lists:nth(Rem,PartitionList),
                  ToSend ! {read,Time,Head},
                  io:format("Sending the following request ~p to ~p ~n ",[{read,Time,Head},ToSend]),
                  snapshot_read(Tail,Time,PartitionList);
    _ ->          io:format("Error in the snapshot_read ~n")
end.


hash(Key) ->
  D = crypto:hash(md4,Key),
  binary_to_integer(D)
.


init() ->
  io:format("Initialization of the transaction manager ~n")

.
