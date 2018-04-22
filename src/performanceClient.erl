%%%-------------------------------------------------------------------
%%% @author gillonb
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Apr 2018 11:53
%%%-------------------------------------------------------------------
-module(performanceClient).
-author("gillonb").

%% API
-export([start/0]).

%% Define the max number of updates to send sequentially
-define(MAX,2000).


start() ->
  io:format("Launch parser client ~n"),
  datastore:start(),
  tmanager:start(manager),
  T1 = os:timestamp(),
  main(1),
  T2 = os:timestamp(),
  MicroSec = timer:now_diff(T2,T1),
  io:format("~nResult: ~p [updates/sec] ~n~n",[(?MAX*1000000)/MicroSec]),
  manager ! {stop}
.

main(Counter) ->
  if Counter > ?MAX ->
    stop;
    true ->
      K = integer_to_list(Counter rem 500),
      Key = "Key",
      FKey = string:concat(Key,K),
      manager ! {update,FKey,13031,self()},
      receive
        {ok} ->
          main(Counter+1)
      end
  end
.