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
  io:format("Opening read file (~p)... ",[Filename]),
  FileI = file:open(Filename, [read]),
  case FileI of
    {ok,Dev} ->
      io:format("ok \n"),
      In = Dev;
    {error, _Reas} ->
      io:format("error \n"),
      case whereis(closedt) of
        undefined ->
          _R = 4;
        _Other ->
          closedt ! {exit}
      end,
      In = none
    %%exit("Error while trying to open file")
  end,
  io:format("Opening write file (~p)... ",[OutFile]),
  FileO = file:open(OutFile, [write]),
  case FileO of
    {ok,IODevice} ->
      io:format("ok \n"),
      Out = IODevice;
    {error, _Reason} ->
      Out = none
      %%exit("Error while trying to open file")
  end,
  W = spawn(parser,writef,[Out]),
  _Temp = register(writes,W),
  io:format("~p~n",[In]),
  S = spawn(parser,execute,[In,TransactionManagerPid]),
  case whereis(closedt) of
    undefined ->
      ok;
    Other ->
      _Reff = erlang:monitor(process, S),
      receive
        {'DOWN', _Ref, process, _Pid2, _Rean} ->
          writes ! {stop},
          Other ! {exit},
          _ = erlang:monitor(process, W),
          receive
            _O ->
              ok
          end
      end
  end
.

%% Take a list of 2 args that are respectively INPUTFILENAME and OUTPUTFILENAME
%%
start_shell(ListArgs) ->
  Filename = lists:nth(1,ListArgs),
  OutputFile = lists:nth(2,ListArgs),
  io:format("Creating 2 datastores and a transaction manager~n"),
  _A = datastore:start(2,data),
  _B = tmanager:start(data,man),
  S = spawn(parser,close,[]),
  _C = register(closedt,S),
  _R = start(Filename,man,OutputFile),
  init:stop()
.

close() ->
  receive
    {exit} ->
      io:format("Closing the stores and transaction manager~n"),
      man ! {exit},
      data ! {exit}
  end
.


execute(D,TransactionManagerPid) ->
  case io:get_line(D, "") of
    eof ->
      file:close(D);
    {error,Reason} ->
      io:format("get_line error: ~p~n",[Reason]);
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



writef(Out) ->
  receive
    {w,Data} ->
      io:fwrite(Out,"~p~n",[Data]),
      writef(Out);
    {writeList,DataList} ->
      format_list(DataList,Out),
      writef(Out);
    {stop} ->
      file:close(Out)
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