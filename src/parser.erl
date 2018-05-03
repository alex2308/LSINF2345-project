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
-export([start/3,execute/2,writef/1,start_shell/1,close/0]).




%%
%% Launch this file to process all transactions in a file
start(Filename,TransactionManagerPid,OutFile) ->
  F = exec(Filename),
  io:format("Opening write file (~p)... ",[OutFile]),
  File = file:open(OutFile, [write]),
  case File of
    {ok,IODevice} ->
      io:format("ok \n"),
      Out = IODevice;
    {error, _Reason} ->
      io:format("error \n"),
      case whereis(closedt) of
        undefined ->
          _R = 4;
        _Other ->
          closedt ! {exit}
      end,
      Out = none,
      exit("Error while trying to open file")
  end,
  W = spawn(parser,writef,[Out]),
  _Temp = register(writes,W),
  _T = spawn(parser,execute,[F,TransactionManagerPid])
.

%% Take a list of 2 args that are respectively INPUTFILENAME and OUTPUTFILENAME
%%
start_shell(ListArgs) ->
  Filename = lists:nth(1,ListArgs),
  OutputFile = lists:nth(2,ListArgs),
  _ = datastore:start(2,data),
  _ = tmanager:start(data,man),
  S = spawn(parser,close,[]),
  _ = register(closedt,S),
  _ = start(Filename,man,OutputFile),
  ok
.

close() ->
  receive
    {exit} ->
      man ! {exit},
      data ! {exit}
  end
.


execute(D,TransactionManagerPid) ->
  case io:get_line(D, "") of
    eof ->
      file:close(D),
      case whereis(closedt) of
        undefined ->
          writes ! {stop};
        _Other ->
          closedt ! {exit},
          writes ! {stop}
      end;

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
          self() ! {skip,ok};
        [Other] ->
          io:format("Skipping unsupported command: ~p ~n",[Other]),
          self() ! {skip,noout},
          execute(D,TransactionManagerPid)
      end,
      receive
        {ok} ->
          writes ! {w,ok},
          execute(D,TransactionManagerPid);
        {ok,Values} ->
          writes ! {writeList,Values},
          execute(D,TransactionManagerPid);
        {skip,O} ->
          case O of
            noout -> execute(D,TransactionManagerPid);
            _ ->
              writes ! {w,ok},
              execute(D,TransactionManagerPid)
          end;
        _Other ->
          execute(D,TransactionManagerPid)
      end
  end
.

exec(InFileName) ->
  io:format("Opening read file (~p)... ",[InFileName]),
  File = file:open(InFileName, [read]),
  case File of
    {ok,IODevice} ->
      io:format("ok \n"),
      IODevice;
    {error, _Reason} ->
      io:format("error \n"),
      case whereis(closedt) of
        undefined ->
          _R = 4;
        _Other ->
          closedt ! {exit}
      end,
      exit("Error while trying to open file")
  end
.



writef(Out) ->
  receive
    {w,Data} ->
      io:fwrite(Out,"~p~n",[Data]),
      writef(Out);
    {writeList,DataList} ->
      format_list(DataList,Out),
      writef(Out);
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