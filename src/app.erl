%%%-------------------------------------------------------------------
%%% @author Bastien Gillon, Alexandre Carlier
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2018 10:00
%%%-------------------------------------------------------------------
-module(app).
-author("Bastien Gillon, Alexandre Carlier").

%% API
-export([gc/1,update/3,snapshotread/2]).


%%
%% Function that sends a update query to a transactional manager
%%  Args: -Tmanager: atom of registered transactional manager
%%        - Key: key value of update query
%%        - Value: value of update query associated with the key
%%
%%  Returns: ok
%%
update(Tmanager,Key,Value) ->
  Tmanager ! {update,Key,Value,self()},
  receive
    {ok} ->
      ok
  end
.

%%
%% Function that sends a snapshot_read query to a transactional manager
%%  Args: -Tmanager: atom of registered transactional manager
%%        - Keys: List if key values we want to have a snapshot from
%%
%%  Returns: ListOfValues, order of values do correspond to keys order
%%
snapshotread(Tmanager,Keys) ->
  Tmanager ! {snapshot_read,Keys,self()},
  receive
    {ok,Values} ->
      Values
  end
.

%%
%% Function that sends a garbage collection query to a transactional manager
%%  Args: -Tmanager: atom of registered transactional manager
%%
%%  Returns: ok
%%
gc(Tmanager) ->
  Tmanager ! {gc,self()},
  receive
    {ok} ->
      ok
  end
.