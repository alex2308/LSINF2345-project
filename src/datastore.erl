%%%-------------------------------------------------------------------
%%% @author Bastien Gillon
%%% @doc
%%%
%%% @end
%%% Created : 19. Apr 2018 11:02
%%%-------------------------------------------------------------------
-module(datastore).
-author("Bastien Gillon").

%% API export
-export([start/0,core/1]).

%% Function run at load of module
%-on_load(init/0).


%% Implemenation
%%
%% -Storage: a dictonary with key and as value a list of tuples. The tuples contain
%%  {Value of key,timestamp}. This list is oreder from new to old timestamps
%%
%% -Main function 'core' is hidden from user. Only interaction happens through 'update' and
%%  'snapshot_read'
%%

%update(Key,Value) ->
%  core ! {update,Key,Value},
%  ok
%.


%snapshot_read(Snapshot_time,Key) ->
  %% Returns the list of Values read at that time snapshot
%  core ! {read,Snapshot_time,Key}
%.
start() ->
  Dict1 = dict:new(),
  Dict2 = dict:new(),
  Data1 = spawn(datastore,core,[Dict1]),
  register(data1,Data1),
  Data2 = spawn(datastore,core,[Dict2]),
  register(data2,Data2)
.

%% Handle update/read etc here. hidden from user.
core(Store) ->
%  if length(ReadBuffer) /= 0 -> % check if we can reduce
%    io:format()
%  end,

  io:format(""),
  receive
    {update,Key,Value} ->
      P = dict:find(Key,Store),
      io:format("Received the following request from Transaction manager: ~p ~n",[{update,Key,Value}]),
      case P of
        {ok,OldHistory} -> % Value already present
          NewHistory = lists:append([{Value,os:timestamp()}],OldHistory), % put the new tuple in front a OldHistory to create new History
          NewStore = dict:store(Key,NewHistory,Store), % Erase old History by overwritting
          core(NewStore);
        error -> % new Value to be stored
          NewStore = dict:store(Key,[{Value,os:timestamp()}],Store),
          core(NewStore)
      end;
    {read,Time,Key} -> % Foreach key in Keys return only valid data!
      P = dict:find(Key,Store),
      io:format("Received the following request from Transaction manager: ~p ~n",[{read,Time,Key}]),
      case P of
        {ok,History} -> %
          io:format("The values for ~p are ~p ~n",[Key,History]),
          T = os:timestamp(),
        if T > Time ->
            io:format("then");
          true ->
            io:format("else")
        end
      end
    end
.

%% Function launched at import module
