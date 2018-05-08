%%%-------------------------------------------------------------------
%%% @author Bastien Gillon, Alexandre Carlier
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2018 11:02
%%%-------------------------------------------------------------------
-module(tmanager).
-author("Bastien Gillon, Alexandre Carlier").

%% API
-export([start_shell/1,start/2,queries/3,response/1,start_node/3]).


%% Function to start when opening tmanager from sh shell not from erlang
%%
%%  ARGS: [DatastorePid,DataStoreNode,Name] => list of strings
%%        - DatastorePid: the atom (in string form) that is the name of the datastore connection handler
%%        - DataStoreNode: the atom (in string form) that is the name of the node where the db is running
%%        - Name: the atom (in string form) you want to assign the new transactional manager to.
%%
start_shell(ListStrArg) ->
  if length(ListStrArg) /= 3 -> io:format("Number of arguments incorrect~n"), exit;
    true ->
      Pid = list_to_atom(lists:nth(1,ListStrArg)),
      Node = list_to_atom(lists:nth(2,ListStrArg)),
      Name = list_to_atom(lists:nth(3,ListStrArg)),
      io:format("~n# tmanager (~p) running on node ~p ~n",[Name,node()]),
      case Node of
        none ->
          start(Pid,Name);
        _Other ->
          _R = start_node(Node,Pid,Name)
      end
  end,
  done
.


%% Function to connect to a data store locate by DatastorePid and launch a transactional manager with name Name
%%
%%  ARGS: - DatastorePid: the Pid of the datastore connection handler
%%        - Name: the atom you want to assign the new transactional manager to.
%%
start(DatastorePid,Name) ->
  DatastorePid ! {connect,self()}, %% connect to the database
  Dd = dict:new(),
  receive
    {ok,ListData} ->
      R = spawn(tmanager,response,[Dd]),
      Core = spawn(tmanager,queries,[0,R,ListData]),
      _F = register(Name,Core)
  end
.

%% Function to run in a live node to connect to other live node
%%
%%  ARGS: - DataStoreNode: the atom that is the name of the node where the db is running
%%        - DatastoreName: the atom that is the name of the datastore connection handler
%%        - Name: the atom you want to assign the new transactional manager to.
%%
start_node(DatastoreNode,DataStoreName,Name) ->
  Succes = net_kernel:connect(DatastoreNode),
  io:format("# Node connected to database node: ~p~n",[Succes]),
  if Succes ->
      io:format("Connected to node ~p~n",[DatastoreNode]),
      start({DataStoreName,DatastoreNode},Name);
    true ->
      io:format("Connect failed to ~p ~n",[DatastoreNode])
  end
.

%% Function that runs in a node and that treats client requests
%%
%%  ARGS: - Counter: Used as Unique Identifier and is increased each time
%%        - Handler: Pid of response handler
%%        - ListOfDatastores: List of Pid of all stores gathered after a connect with data store
%%
queries(Counter,Handler,ListOfDatastores) ->
  receive
    {update,Key,Value,ClientPid} ->
      Handler ! {add,Counter,ClientPid}, % notify response handler of future incoming message from a store
      Rem = hash(Key,length(ListOfDatastores)),
      lists:nth(Rem,ListOfDatastores) ! {update,Key,Value,Handler,Counter}, % send update query to right store
      queries(Counter+1,Handler,ListOfDatastores);
    {snapshot_read,Keys,ClientPid} ->
      %io:format("snapshot_read request from client ~n"),
      snapshot_read_all(Keys,Counter,Handler,ClientPid,ListOfDatastores),% notify response handler + send snapshot_read to correct store
      queries(Counter+1,Handler,ListOfDatastores);
    {gc,ClientPid} ->
      NewC = send_gc_all(Counter,ListOfDatastores,ClientPid,Handler),
      Handler ! {add,Counter,{ClientPid,length(ListOfDatastores)}}, % notify response handler of future incoming message from a store
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
%%  ARGS: - Awaits: Dictionnary of a Unique Identifier (key) and a value that
%%            can be a clientPid (for update) or a tuple of clientPid and a integer representing all the
%%            datastore that need to respond (for gc)
%%            or a tuple of clientPid,NumberOfResponseWaiting and empty dictionnary (which  will contain
%%            the index from the snapshot_read request as key and as value the value read from the store)
%%            (for snapshot read)
%%
%%
response(Awaits) ->
  receive
    {add,Key,Value} ->
      %io:format("Store new ~n"),
      NewAwaits = dict:store(Key,Value,Awaits), % store the fact that we should await a response
      response(NewAwaits);
    {ok,update,Id} -> % store has sucessfully handled update
      %io:format("Handle update-reponse from Datastore ~n"),
      case dict:find(Id,Awaits) of
        {ok,ClientPiD} ->
          ClientPiD ! {ok}, % reply ok to client
          response(dict:erase(Id,Awaits)); % remove answered query and reloop;
        error ->
          %io:format("Error no awaiting Id (~p) for response from datastore ~n",[Id]),
          response(Awaits)
      end;
    {ok,read,Uid,Index,Value} -> % store has sucessfully handled snapshot_read
      %io:format("Handle read-reponse from Datastore~n"),
      case dict:find(Uid,Awaits) of
        {ok,Tuple} ->
          NTuple = process_read_response(Tuple,Index,Value), % update tuple that stores info about snapshot_reads
          if (element(2,NTuple) == 0) -> % all snapshot_reads done
              List = lists:sort(fun (A,B) -> element(1,A) =< element(1,B) end,dict:to_list(element(3,NTuple))),
              element(1,NTuple) ! {ok,lists:map(fun(A) -> element(2,A) end,List)}, % send Values to client
              response(dict:erase(Uid,Awaits));
            true -> % still some snapshot_reads undone
              response(dict:store(Uid,NTuple,Awaits))
          end;
        error ->
          %io:format("Error no awaiting Id (~p) for response from datastore ~n",[Id]),
          response(Awaits)
      end;
    {ok,gc,Id} -> % store has sucessfully handled gc
      case dict:find(Id,Awaits) of
        {ok,{ClientPiD,N}} ->
          if (N==1) -> % All stores have replied
            ClientPiD ! {ok},
            response(dict:erase(Id,Awaits)); % remove gc query waiting
          true -> % still some stores that have not replied
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


%%%%% AUXILIARY FUNCTIONS %%%%%
%% Simple hash function to distributed data over N stores
%%
hash(Key,N) ->
  erlang:phash2(Key,N)+1
.

snapshot_read_all(Keys,Uid,HandleResp,ClientPid,ListOfDatastores) ->
  HandleResp ! {add,Uid,{ClientPid,length(Keys),dict:new()}}, % send new response waiter to response handler
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
  Rem = hash(Key,length(ListOfDatastores)),
  lists:nth(Rem,ListOfDatastores) ! {read,T,Key,HandleResp,Uid,Index} % send snapshot_read query to right store
.


%% Add snapshot_read response to dictionnary and return new tuple
%%  => tuple is of from {ClientPid,#reponsewaiting,dict(Index,Value)}
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


%% Return the new counter value after having send gc to all stores
send_gc_all(Counter,ListOfDatastores,ClientPid,Handler) ->
  case ListOfDatastores of
      []-> Counter+1;
      [H|T] ->
        H ! {gc,Handler,Counter},
        send_gc_all(Counter,T,ClientPid,Handler)
  end
.