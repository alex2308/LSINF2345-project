%%%-------------------------------------------------------------------
%%% @author gillonb
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2018 11:02
%%%-------------------------------------------------------------------
-module(tmanager).
-author("gillonb").

%% API
-export([start/1,queries/2,response/1]).


-import(datastore,[start/0]).


hash(Key) ->
  erlang:phash2(Key)
.


start(Name) ->
  case whereis(data1) of
    undefined ->
      io:format("Starting 2 datastores....."),
      datastore:start(),
      io:format("Running~n");
    _Other ->
      io:format("")
  end,
  case Name of
    [H|_T] ->
      Reg = H;
    Other ->
      Reg = Other
  end,
  Dd = dict:new(),
  R = spawn(tmanager,response,[Dd]),
  Core = spawn(tmanager,queries,[0,R]),
  if is_atom(Reg) ->
      register(Reg,Core),
      _P = io:format("Transaction manager running and registered as ~p~n",[Reg]);
    true ->
      O = list_to_atom(Reg),
      register(O,Core),
      _P = io:format("Transaction manager running and registered as ~p~n",[O])
  end,
  io:format("NODE: ~p~n~n",[node()]),
  done
.

%% Function that runs in a node and that treats client requests
%%
queries(Counter,Handler) ->
  receive
    {update,Key,Value,ClientPid} ->
      Handler ! {add,Counter,ClientPid}, % Add response handler for Client request
      Rem = hash(Key) rem 2,
      if Rem == 1 ->
          data1 ! {update,Key,Value,Handler,Counter};
          %io:format("Sending the following request ~p to ~p ~n",[{update,Key,Value},'data1']);
        true ->
          data2 ! {update,Key,Value,Handler,Counter}
          %io:format("Sending the following request ~p to ~p ~n",[{update,Key,Value},'data2'])
      end,
      queries(Counter+1,Handler);
    {snapshot_read,Keys,ClientPid} ->
      %io:format("snapshot_read request from client ~n"),
      snapshot_read_all(Keys,Counter,Handler,ClientPid),
      queries(Counter+1,Handler);
    {gc,ClientPid} ->
      Handler ! {add,Counter,ClientPid},
      data1 ! {gc,Handler,Counter},
      Handler ! {add,Counter,ClientPid},
      data2 ! {gc,Handler,Counter},
      queries(Counter+1,Handler);
    {stop} ->
      data1 ! {stop},
      data2 ! {stop},
      Handler ! {stop},
      io:format("Stop query manager ~n"),
      exit('exit')
  end
.


%%
%% Function that runs in a node and that treats the responses to the client
%%
%%  Awaits is DICT of key,value (UNQIUE KEY (counter number),CLIENTPID)
response(Awaits) ->
  receive
    {add,Key,Value} ->
      %io:format("Store new ~n"),
      NewAwaits = dict:store(Key,Value,Awaits),
      response(NewAwaits);
    {ok,update,Id} ->
      %io:format("Handle update-reponse from Datastore ~n"),
      case dict:find(Id,Awaits) of
        {ok,ClientPiD} ->
          ClientPiD ! {ok},
          response(dict:erase(Id,Awaits)); % remove answered query and reloop;
        error ->
          %io:format("Error no awaiting Id (~p) for response from datastore ~n",[Id]),
          response(Awaits)
      end;
    {ok,read,Uid,Index,Value} ->
      %io:format("Handle read-reponse from Datastore~n"),
      case dict:find(Uid,Awaits) of
        {ok,Tuple} ->
          NTuple = process_read_response(Tuple,Index,Value),
          if (element(2,NTuple) == 0) ->
              List = lists:sort(fun (A,B) -> element(1,A) =< element(1,B) end,dict:to_list(element(3,NTuple))),
              element(1,NTuple) ! {ok,lists:map(fun(A) -> element(2,A) end,List)},
              response(dict:erase(Uid,Awaits));
            true ->
              io:format(""),
              response(dict:store(Uid,NTuple,Awaits))
          end;
        error ->
          %io:format("Error no awaiting Id (~p) for response from datastore ~n",[Id]),
          response(Awaits)
      end;
    {ok,gc,Id} ->
      case dict:find(Id,Awaits) of
        {ok,ClientPiD} ->
          ClientPiD ! {ok},
          response(dict:erase(Id,Awaits)); % remove answered query and reloop
        error ->
          %io:format("Error no awaiting Id (~p) for response from datastore ~n",[Id]),
          response(Awaits)
      end;
    {stop} ->
      io:format("Stop response manager ~n")
  end
.

snapshot_read_all(Keys,Uid,HandleResp,ClientPid) ->
  HandleResp ! {add,Uid,{ClientPid,length(Keys),dict:new()}},
  snap(Keys,Uid,HandleResp,1)
.

snap(Keys,Uid,HandleResp,Index) ->
  case Keys of
    [] ->
      io:format("");
    [H|T] ->
      snapshot_read(H,Uid,HandleResp,Index),
      snap(T,Uid,HandleResp,Index+1)
  end
.

snapshot_read(Key,Uid,HandleResp,Index) ->
  T = os:timestamp(),
  %% Returns the list of Values read at that time snapshot
  Rem = hash(Key) rem 2, %pour simplifier j'ai juste fait le reste de la division par 2
  if Rem == 1 ->
      data1 ! {read,T,Key,HandleResp,Uid,Index};
      %io:format("Sending the following request ~p to ~p ~n",[{read,T,Key},data1]);
    true ->
      data2 ! {read,T,Key,HandleResp,Uid,Index}
      %io:format("Sending the following request ~p to ~p ~n",[{read,T,Key},data2])
  end
.


%% Wait is tuple {ClientPid,#reponsewaiting,dict(Index,Value)}
process_read_response(Wait,Index,Value) ->
  %% add new value to Wait + check if not already present => error else OK.
  Dict = element(3,Wait),
  case dict:find(Index,Dict) of
    error ->  %% good as no 2 responses for same Index can be seen
      NewDict = dict:store(Index,Value,Dict),
      {element(1,Wait),element(2,Wait)-1,NewDict};
    {ok,_V} ->
      io:format("Error got response for Index already present ~n"),
      Wait
  end
.

%% lists:sort(fun (A,B) -> element(1,A) =< element(1,B) end,dict:to_list(D5))
%% lists:map(fun (A) -> element(2,A) end,LIST)