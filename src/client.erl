%%%-------------------------------------------------------------------
%%% @author Bastien Gillon
%%% @doc
%%%
%%% @end
%%% Created : 20. Apr 2018 8:56
%%%-------------------------------------------------------------------
-module(client).
-author("Bastien Gillon").

%% API
-export([startT/1]).

-import(parser,[start/1]).



startT(Filename) ->
  io:format("Launch parser client ~n"),
  datastore:start(),
  tmanager:start(manager),
  parser:start(Filename),
  manager ! {stop}
.