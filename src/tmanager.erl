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
-export([update/2,snapshot_read/1]).

%% Function run at load of module
-on_load(init/0).



update(Key,Value) ->
  %% Find in which datastore the Key is stored (HASH)
  %V = hash(Key),
  Rem = Key rem 2, %pour simplifier j'ai juste fait le reste de la division par 2
  if Rem == 1 ->
    data1 ! {update,Key,Value},
    io:format("Sending the following request ~p to ~p ~n ",[{update,Key,Value},'data1']);
    true ->
    data2 ! {update,Key,Value},
    io:format("Sending the following request ~p to ~p ~n ",[{update,Key,Value},'data2'])
  end,
  ok
.


snapshot_read(Key) ->
  T = os:timestamp(),
  %% Returns the list of Values read at that time snapshot
  Rem = Key rem 2, %pour simplifier j'ai juste fait le reste de la division par 2
  if Rem == 1 ->
    data1 ! {read,T,Key},
    io:format("Sending the following request ~p to ~p ~n ",[{read,T,Key},data1]);
    true ->
    data2 ! {read,T,Key},
    io:format("Sending the following request ~p to ~p ~n ",[{read,T,Key},data2])
  end,
  ok
.


hash(Key) ->
  D = crypto:hash(md4,Key),
  binary_to_integer(D)
.


init() ->
  io:format("Initialization of the transaction manager ~n")

.
