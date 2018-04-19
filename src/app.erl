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
-export([]).


%%%
%%% TODO: update key: insert if new, else update (new timestamped)
%%%
%%%

dataserver() ->
  receive
    {Pid,query} ->
      io:format("")
  end.

tmanager() ->
  io:format("")
.

client() ->
  io:format("I'm client")
.