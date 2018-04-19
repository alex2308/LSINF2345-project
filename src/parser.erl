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
-export([start/1]).

-import(tmanager,[snapshot_read/1,update/2]).

start(FileName) ->
  io:format("Do read and execute \n"),
  File = file:open(FileName, [read]),
  case File of
    {ok,IODevice} ->
      io:format("File opened... \n"),
      try execute_lines(IODevice)
      after file:close(IODevice)
      end;

    {error, Reason} ->
      io:format("Error opening file \n")
  end
.

execute_lines(Device) ->
  case io:get_line(Device, "") of
    eof  -> ok;
    Line ->
      Chars = string:split(Line, " "),
      case Chars of
        ["up",Key,Value] ->
          io:format("Send update request for Key:~p and Value:~p \n",[Key,Value]),
          tmanager:update(Key,Value);
        ["read",T] ->
          io:format("Send read request for Keys ");
        ["gc"] ->
          io:format("Do garbage collectin \n");
        ["sleep",TimeStr] ->
          Time = string:to_integer(TimeStr),
          timer:sleep(Time), %% in milliseconds
          io:format("Sleep \n");
        Other ->
          nil
      end,
    execute_lines(Device)
  end
.