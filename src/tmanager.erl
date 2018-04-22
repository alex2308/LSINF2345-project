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



snapshot_read(Key,Uid,HandleResp,ClientPid) ->
  T = os:timestamp(),
  %% Returns the list of Values read at that time snapshot
  Rem = hash(Key) rem 2, %pour simplifier j'ai juste fait le reste de la division par 2
  if Rem == 1 ->
      HandleResp ! {add,Uid,ClientPid}, % Add response handler for Client request
      data1 ! {read,T,Key,HandleResp,Uid};
      %io:format("Sending the following request ~p to ~p ~n",[{read,T,Key},data1]);
    true ->
      HandleResp ! {add,Uid,ClientPid},
      data2 ! {read,T,Key,HandleResp,Uid}
      %io:format("Sending the following request ~p to ~p ~n",[{read,T,Key},data2])
  end,
  ok
.


hash(Key) ->
  erlang:phash2(Key)
.


start(Name) ->
  Dd = dict:new(),
  R = spawn(tmanager,response,[Dd]),
  Core = spawn(tmanager,queries,[0,R]),
  register(Name,Core)
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
      snapshot_read(Keys,Counter,Handler,ClientPid),
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
      io:format("Stop query manager ~n")
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
    {ok,read,Values,Id} ->
      %io:format("Handle read-reponse from Datastore~n"),
      case dict:find(Id,Awaits) of
        {ok,ClientPiD} ->
          ClientPiD ! {ok,Values},
          response(dict:erase(Id,Awaits)); % remove answered query and reloop
        error ->
          %io:format("Error no awaiting Id (~p) for response from datastore ~n",[Id]),
          response(Awaits)
      end;
    {ok,gc,Id} ->
      case dict:find(Id,Awaits) of
        {ok,ClientPiD} ->
          ClientPiD ! {ok,gc},
          response(dict:erase(Id,Awaits)); % remove answered query and reloop
        error ->
          %io:format("Error no awaiting Id (~p) for response from datastore ~n",[Id]),
          response(Awaits)
      end;
    {stop} ->
      io:format("Stop response manager ~n")
  end
.