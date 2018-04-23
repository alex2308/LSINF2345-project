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
-export([start_service/0,stop_service/0,send_update/2,send_snapshotread/1,do_gc/0]).


%% Start 2 stores and 1 manager with name manager
start_service() ->
  case whereis(manager) of
    undefined ->
      datastore:start(),
      tmanager:start(manager);
    _Other ->
      io:format("Service already running~n"),
      done
  end
.

stop_service() ->
  case whereis(manager) of
    undefined ->
      io:format("Service not running~n"),
      done;
    Other ->
      Other ! {stop}
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