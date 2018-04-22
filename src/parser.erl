%%%-------------------------------------------------------------------
%%% @author Bastien Gillon
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2018 17:50
%%%-------------------------------------------------------------------
-module(parser).
-author("Bastien Gillon").

%% API
-export([start/1,execute/1]).

execute(D) ->
  case io:get_line(D, "") of
    eof ->
      io:format("DONE~n"),
      file:close(D);
    Line ->
      Chars = string:split(string:trim(Line)," ",all),
      case Chars of
        ["up",Key,Value] ->
          manager ! {update,Key,Value,self()};
        ["read" | Keys] ->
          manager ! {snapshot_read,Keys,self()};
        ["gc"] ->
          manager ! {gc,self()};
        ["sleep",StrTime] ->
          T = string:to_integer(StrTime),
          case T of
            {Time,Rest} ->
              Done = timer:sleep(Time);
            {error,Reason} ->
              io:format("badarg sleep~n")
          end,
          self() ! {skip}
      end,
      receive
        {ok} ->
          io:format("ok~n"),
          execute(D);
        {ok,Values} ->
          io:format("Values~n"),
          execute(D);
        {ok,gc} ->
          io:format("GC ok~n"),
          execute(D);
        {skip} ->
          execute(D);
        Other ->
          io:format("Unknown~n")
      end
  end
.

exec(FileName) ->
  io:format("Do read and execute \n"),
  File = file:open(FileName, [read]),
  case File of
    {ok,IODevice} ->
      io:format("File opened... \n"),
      IODevice;
    {error, Reason} ->
      io:format("Error opening file \n")
  end
.

start(Filename) ->
  F = exec(Filename),
  T = spawn(parser,execute,[F])
.