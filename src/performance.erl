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
-export([start/2,time_exec/2]).

%% Define the max number of updates to send sequentially
-define(MAX,2000).


start(N,ManagerPid) ->
  io:format("Launch ~p parser client(s) ~n",[N]),
  run(N,self(),ManagerPid),
  get_all(N)
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
        -> io:format("~nResult: ~p [updates/sec] ~n~n",[(?MAX*1000000)/TimeVal]),get_all(N-1)
    end;
    true ->
      ok
  end
.
time_exec(Pid,ManagerPid) ->
  T1 = os:timestamp(),
  main(1,ManagerPid),
  T2 = os:timestamp(),
  MicroSec = timer:now_diff(T2,T1),
  Pid ! {ok,MicroSec}
.


main(Counter,ManagerPid) ->
  if Counter > ?MAX ->
    stop;
    true ->
      K = integer_to_list(Counter rem 500),
      Key = "Key",
      FKey = string:concat(Key,K),
      ManagerPid ! {update,FKey,13031,self()},
      receive
        {ok} ->
          main(Counter+1,ManagerPid)
      end
  end
.