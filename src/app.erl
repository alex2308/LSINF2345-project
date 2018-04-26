%%%-------------------------------------------------------------------
%%% @author Bastien Gillon
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2018 10:00
%%%-------------------------------------------------------------------
-module(app).
-author("Bastien Gillon").

%% API
-export([gc/1,update/3,snapshotread/2]).



update(Tmanager,Key,Value) ->
  Tmanager ! {update,Key,Value,self()},
  receive
    {ok} ->
      ok
  end
.

snapshotread(Tmanager,Keys) ->
  Tmanager ! {snapshot_read,Keys,self()},
  receive
    {ok,Values} ->
      Values
  end
.

gc(Tmanager) ->
  Tmanager ! {gc,self()},
  receive
    {ok} ->
      ok
  end
.