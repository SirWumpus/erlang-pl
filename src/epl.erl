-module(epl).
-export([get_path/2, get_path/3, set_path/3, set_paths/2, is_plist/1]).

-type path() :: [atom()].
-export_type([path/0]).

%% get_path(Plist, Keys) -> Value | undefined
%%
%% Given a Path into Plist0, return the value found; otherwise undefined
%% if not found.
%%
-spec get_path(Plist :: proplists:proplist(), Keys :: path()) -> any() | undefined.
get_path(Plist, Keys) ->
    get_path(Plist, Keys, undefined).

%% get_path(Plist, Keys, Default) -> Value | Default
%%
%% Given a Path into Plist0, return the value found; otherwise a Default
%% value if not found.
%%
-spec get_path(Plist :: proplists:proplist(), Keys :: path(), Default :: any()) -> any().
get_path(undefined, _Keys, Default) ->
    Default;
get_path(Plist, [], _Default) ->
    Plist;
get_path(Plist, [Key | Keys], Default) ->
    get_path(proplists:get_value(Key, Plist), Keys, Default).

%% set_path(Plist0, Path, Value) -> Plist1
%%
%% Given a Path into Plist0, insert or replace a value.  If the Path
%% specified does not exist, it will be created.
%%
-spec set_path(Plist :: proplists:proplist(), Path :: path(), Value :: any()) -> proplists:proplist().
set_path(Plist, [Key], Value) ->
    % Last key, update its value.
    [{Key, Value} | proplists:delete(Key, Plist)];
set_path(Plist, [Key | Keys], Value) ->
    case proplists:get_value(Key, Plist) of
    undefined ->
        % Path not defined and more keys to go.
        [{Key, set_path([], Keys, Value)} | Plist];
    SubList ->
        [{Key, set_path(SubList, Keys, Value)} | proplists:delete(Key, Plist)]
    end.

%% set_paths(Plist0, PathsValues) -> Plist1
%%
%% Given a list of paths and values, insert or replace one or
%% more values in Plist0.  See set_path/3.
%%
-spec set_paths(Plist :: proplists:proplist(), PathsValues :: [{path(), any()}]) -> proplists:proplist().
set_paths(Plist, PathsValues) ->
    lists:foldl(fun ({Path, Value}, Acc) ->
        set_path(Acc, Path, Value)
    end, Plist, PathsValues).

% X = [{a, 1}, {b, [{c, [{d, 2},{e,3}]}]}].
% epl:set_paths(X, [{[b,c,z],123},{[z,y,x],321}]).
% [{z,[{y,[{x,321}]}]},{b,[{c,[{z,123},{d,2},{e,3}]}]},{a,1}]

%% is_plist(Thing) -> Bool
%%
%% Return true if Thing adheres to a formal definition of a Plist
%% (suitable to convert to JSON), where each list element is either
%% an atom (short hand of `{Atom, true}`) or 2-tuple Key-Value.
%%
-spec is_plist(Thing :: term()) -> boolean().
is_plist(Thing) when is_list(Thing) ->
	lists:all(fun (Item) ->
		is_atom(Item) orelse (is_tuple(Item) andalso tuple_size(Item) == 2)
	end, Thing);
is_plist(_Other) ->
	false.
