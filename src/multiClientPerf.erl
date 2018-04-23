%%%-------------------------------------------------------------------
%%% @author gillonb
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Apr 2018 12:12
%%%-------------------------------------------------------------------
-module(multiClientPerf).
-author("gillonb").

%% API
-export([start/0]).

-define(MaxReq,2000).

-define(MaxClient,100).


start() ->
  datastore:start(),
  tmanager:start(manager),
  start(0)
.
start(N) ->
  if N > 2000 ->
    io:format("Done spawning 2000~n");
    true ->
      _P = spawn(multiClientPerf,main,[N]),
      start(N+1)
  end
.

main(Counter) ->
  K = integer_to_list(Counter),
  Key = "Key",
  FKey = string:concat(Key,K),
  manager ! {update,FKey,13031,self()},
  receive
    {ok} ->
      done
  end
.

client(Counter) ->
  if Counter > ?MaxReq ->
    stop;
    true ->
      K = integer_to_list(Counter),
      Key = "Key",
      FKey = string:concat(Key,K),
      manager ! {update,FKey,13031,self()},
      receive
        {ok} ->
          client(Counter+1)
      end
  end
.