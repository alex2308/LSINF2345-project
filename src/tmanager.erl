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
-export([startshell/1,start/2,queries/3,response/1,start_node/3]).


-import(datastore,[start/0]).


hash(Key,N) ->
  erlang:phash2(Key,N)+1
.

startshell(ListStrArg) ->
  if length(ListStrArg) /= 3 -> io:format("Number of arguments incorrect~n"), exit;
    true ->
      Pid = list_to_atom(lists:nth(1,ListStrArg)),
      Node = list_to_atom(lists:nth(2,ListStrArg)),
      Name = list_to_atom(lists:nth(3,ListStrArg)),
      S = net_kernel:connect(Node),
      io:format("~n# tmanager (~p) running on node ~p ~n",[Name,node()]),
      io:format("# Node connected to database node: ~p~n",[S]),
      case Node of
        none ->
          start(Pid,Name);
        _Other ->
          start({Pid,Node},Name)
      end
  end
.

start(DatastorePid,Name) ->
  DatastorePid ! {connect,self()}, %% connect to the database
  Dd = dict:new(),
  receive
    {ok,ListData} ->
      R = spawn(tmanager,response,[Dd]),
      Core = spawn(tmanager,queries,[0,R,ListData]),
      _F = register(Name,Core)
  end,
  done
.

%% Function to run in a live node to connect to other live node
%
start_node(DatastoreNode,DataStoreName,Name) ->
  Succes = net_kernel:connect(DatastoreNode),
  io:format("~p~n",[Succes]),
  if Succes ->
      io:format("Connected to node ~p~n",[DatastoreNode]),
      start({DataStoreName,DatastoreNode},Name);
    true ->
      io:format("Connect failed to ~p ~n",[DatastoreNode]),
      done
  end
.

%% Function that runs in a node and that treats client requests
%%
queries(Counter,Handler,ListOfDatastores) ->
  receive
    {update,Key,Value,ClientPid} ->
      Handler ! {add,Counter,ClientPid}, % Add response handler for Client request
      Rem = hash(Key,length(ListOfDatastores)),
      lists:nth(Rem,ListOfDatastores) ! {update,Key,Value,Handler,Counter},
      queries(Counter+1,Handler,ListOfDatastores);
    {snapshot_read,Keys,ClientPid} ->
      %io:format("snapshot_read request from client ~n"),
      snapshot_read_all(Keys,Counter,Handler,ClientPid,ListOfDatastores),
      queries(Counter+1,Handler,ListOfDatastores);
    {gc,ClientPid} ->
      NewC = send_gc_all(Counter,ListOfDatastores,ClientPid,Handler),
      Handler ! {add,Counter,{ClientPid,length(ListOfDatastores)}},
      queries(NewC,Handler,ListOfDatastores);
    {exit} ->
      Handler ! {exit},
      io:format("Query manager terminated~n"),
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
        {ok,{ClientPiD,N}} ->
          if (N==1) ->
            ClientPiD ! {ok}, %% only when all have replied!!
            response(dict:erase(Id,Awaits)); % remove gc query waiting
          true ->
            response(dict:store(Id,{ClientPiD,N-1},Awaits)) % remove 1 answered query
          end;
        error ->
          %io:format("Error no awaiting Id (~p) for response from datastore ~n",[Id]),
          response(Awaits)
      end;
    {exit} ->
      io:format("Response manager terminated~n")
  end
.

snapshot_read_all(Keys,Uid,HandleResp,ClientPid,ListOfDatastores) ->
  HandleResp ! {add,Uid,{ClientPid,length(Keys),dict:new()}},
  snap(Keys,Uid,HandleResp,1,ListOfDatastores)
.

snap(Keys,Uid,HandleResp,Index,ListOfDatastores) ->
  case Keys of
    [] ->
      io:format("");
    [H|T] ->
      snapshot_read(H,Uid,HandleResp,Index,ListOfDatastores),
      snap(T,Uid,HandleResp,Index+1,ListOfDatastores)
  end
.

snapshot_read(Key,Uid,HandleResp,Index,ListOfDatastores) ->
  T = os:timestamp(),
  %% Returns the list of Values read at that time snapshot
  Rem = hash(Key,length(ListOfDatastores)), %pour simplifier j'ai juste fait le reste de la division par 2
  lists:nth(Rem,ListOfDatastores) ! {read,T,Key,HandleResp,Uid,Index}
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


%% Return the new counter value
send_gc_all(Counter,ListOfDatastores,ClientPid,Handler) ->
  case ListOfDatastores of
      []-> Counter+1;
      [H|T] ->
        H ! {gc,Handler,Counter},
        send_gc_all(Counter,T,ClientPid,Handler)
  end
.