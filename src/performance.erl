%%%-------------------------------------------------------------------
%%% @author gillonb
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Apr 2018 11:53
%%%-------------------------------------------------------------------
-module(performance).
-author("gillonb").

%% API
-export([start_loop/2,start/2,time_exec/2,test_gc/1]).

%% Define the max number of updates to send sequentially
-define(MAX,2000).
start_loop(ListNumbers,ManagerPid) ->
  case ListNumbers of
    [H|T] -> start(H,ManagerPid),start_loop(T,ManagerPid);
    [] -> ok
  end.
test_gc(ManagerPid) ->
  T1 = os:timestamp(),
  app:gc(ManagerPid),
  T2 = os:timestamp(),
MicroSec = timer:now_diff(T2,T1),
io:format("Time to empty with GC is ~p ~n",[MicroSec]).

%% Function to measure performance of a db system.
%%  Each client sends an update query and waits for a response before repeating.
%%
%%  ARGS: -N: number of clients run in parallel that will issue updates
%%        -ManagerPid: atom of transactional manager Pid
%%
%% NOTE: clients send unique keys but with parallel clients, they maybe update keys from others
start(N,ManagerPid) ->
  io:format("Launch ~p parser client(s) ~n",[N]),
  run(N,self(),ManagerPid),
  get_all(N),
  Result = average_time(N),
  io:format("Average time to read is ~p ~n",[Result/N])
.

run(N,P,ManagerPid) ->
  if (N /= 0) ->
    spawn(performance,time_exec,[P,ManagerPid]),
    run(N-1,P,ManagerPid);
    true ->
      ok
  end
.

get_all(N) ->
  if (N /= 0) ->
    receive
      {ok,TimeVal}
        -> get_all(N-1)
        %io:format("~nResult: ~p [updates/sec] ~n~n",[(?MAX*1000000)/TimeVal]),
    end;
    true ->
      ok
  end
.
average_time(N) ->
    if (N /= 0) ->
    receive
      {ok,Val,read}
        -> Val/1000 + average_time(N-1)
    end;
    true ->
      0
  end
.
time_exec(Pid,ManagerPid) ->
  T1 = os:timestamp(),
  main(1,ManagerPid),
  T2 = os:timestamp(),
  KeyCreator = fun(X) ->
  string:concat("Key",integer_to_list(X))
  end,
 List = lists:map(KeyCreator,lists:seq(1,400)),
 T3 = os:timestamp(),
  Answer = app:snapshotread(ManagerPid,List),
  T4 = os:timestamp(),
  MicroSec = timer:now_diff(T2,T1),
  MicroSec2 = timer:now_diff(T4,T3),
  Pid ! {ok,MicroSec},
  Pid ! {ok,MicroSec2,read}
.


main(Counter,ManagerPid) ->
  if Counter > ?MAX ->
    stop;
    true ->
      K = integer_to_list(Counter),
      Key = "Key",
      FKey = string:concat(Key,K),
      Value = rand:uniform(100000),
      ManagerPid ! {update,FKey,Value,self()},
      receive
        {ok} ->
          main(Counter+1,ManagerPid)
      end
  end
.
