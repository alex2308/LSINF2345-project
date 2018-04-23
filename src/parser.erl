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
-export([start/3,execute/2,write/1]).



%%
%% Launch this file to process all transactions in a file
start(Filename,TransactionManagerPid,OutFile) ->
  F = exec(Filename),
  File = file:open(OutFile, [write]),
  case File of
    {ok,IODevice} ->
      io:format("File opened... \n"),
      Out = IODevice;
    {error, _Reason} ->
      io:format("Error opening file \n"),
      Out = none
  end,
  W = spawn(parser,write,[Out]),
  _Temp = register(write,W),
  _T = spawn(parser,execute,[F,TransactionManagerPid])
.


execute(D,TransactionManagerPid) ->
  case io:get_line(D, "") of
    eof ->
      file:close(D),
      write ! {stop};
    Line ->
      Chars = string:split(string:trim(Line)," ",all),
      case Chars of
        ["up",Key,Value] ->
          TransactionManagerPid ! {update,Key,Value,self()};
        ["read" | Keys] ->
          TransactionManagerPid ! {snapshot_read,Keys,self()};
        ["gc"] ->
          TransactionManagerPid ! {gc,self()};
        ["sleep",StrTime] ->
          T = string:to_integer(StrTime),
          case T of
            {error,_Reason} ->
              io:format("badarg sleep~n");
            {Time,_Rest} ->
              _Done = timer:sleep(Time)
          end,
          self() ! {skip}
      end,
      receive
        {ok} ->
          write ! {w,ok},
          execute(D,TransactionManagerPid);
        {ok,gc} ->
          write ! {w,ok},
          execute(D,TransactionManagerPid);
        {ok,Values} ->
          write ! {writeList,Values},
          execute(D,TransactionManagerPid);
        {skip} ->
          write ! {w,ok},
          execute(D,TransactionManagerPid);
        _Other ->
          io:format("Unknown~n")
      end
  end
.

exec(InFileName) ->
  io:format("Do read and execute \n"),
  File = file:open(InFileName, [read]),
  case File of
    {ok,IODevice} ->
      io:format("File opened... \n"),
      IODevice;
    {error, _Reason} ->
      io:format("Error opening file \n")
  end
.



write(Out) ->
  receive
    {w,Data} ->
      io:fwrite(Out,"~p~n",[Data]),
      write(Out);
    {writeList,DataList} ->
      format_list(DataList,Out),
      write(Out);
    {stop} ->
      file:close(Out),
      done
  end
.

format_list(L,Out)
  when is_list(L) ->
    io:fwrite(Out,"[",[]),
    fnl(L,Out),
    io:fwrite(Out,"]~n",[])
.

fnl([H],Out) ->
  io:fwrite(Out,"~s", [H]);
fnl([H|T],Out) ->
  io:fwrite(Out,"~s,", [H]),
  fnl(T,Out);
fnl([],_Out) ->
  ok
.