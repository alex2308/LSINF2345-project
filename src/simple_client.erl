%%%-------------------------------------------------------------------
%%% @author gillonb
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2018 8:56
%%%-------------------------------------------------------------------
-module(simple_client).
-author("gillonb").

%% API
-export([start/0]).




start() ->
  io:format("Launch simple client ~n"),
  datastore:start(),
  tmanager:start(manager),
  manager ! {update,"Key1",4242,self()},
  receive
    {ok} ->
      io:format("ANSWER: ok ~n")
  end,
  manager ! {stop}
.