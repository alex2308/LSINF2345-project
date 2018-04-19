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
  V = hash(Key),
  Rem = V rem 2,
  if Rem == 1 ->
    io:format("Storage 1");
    true ->
      io:format("Storage 2")
  end,
  ok
.


snapshot_read(Keys) ->
  %% Returns the list of Values read at that time snapshot
  [""]
.


hash(Key) ->
  D = hash(md4,Key),
  binary_to_integer(D)
.


init() ->
  io:format("")

.