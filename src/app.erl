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
-export([start_local_service/0,stop_local_service/0,send_update/2,
  send_snapshotread/1,do_gc/0,send_update_external/3,send_snapshotread_external/2]).


%% Start 2 stores and 1 manager with name manager
start_local_service() ->
  case whereis(manager) of
    undefined ->
      datastore:start(),
      tmanager:start(manager);
    _Other ->
      io:format("Service already running~n"),
      done
  end
.

stop_local_service() ->
  case whereis(manager) of
    undefined ->
      io:format("Service not running~n"),
      done;
    Other ->
      Other ! {stop}
  end
.

send_update_external(Node,Key,Value) ->
  {manager,Node} ! {update,Key,Value,self()},
  receive
    {ok} ->
      ok
  end
.

send_snapshotread_external(Node,Keys) ->
  {manager,Node} ! {snapshot_read,Keys,self()},
  receive
    {ok,Values} ->
      Values
  end
.

send_update(Key,Value) ->
  case whereis(manager) of
    undefined ->
      io:format("Service not running~n"),
      done;
    Other ->
      Other ! {update,Key,Value,self()},
      receive
        {ok} ->
          ok
      end
  end
.

send_snapshotread(Keys) ->
  case whereis(manager) of
    undefined ->
      io:format("Service not running~n"),
      done;
    Other ->
      Other ! {snapshot_read,Keys,self()},
      receive
        {ok,Values} ->
          Values
      end
  end
.

do_gc() ->
  case whereis(manager) of
    undefined ->
      io:format("Service not running~n"),
      done;
    Other ->
      Other ! {gc,self()},
      receive
        {ok} ->
          ok
      end
  end
.