-module(modelo).

-export([search/2]).

%-fields([]).

init(Table, Opts) -> 
	mnesia:create_table(Table, [{fields, getFields(Table)}|Opts]).

find(Table, Key) ->
	[KeyField | _Rest] = getFields(Table),
	Match = erlang:make_tuple(2, [Table, KeyField]),
	mnesia:transaction(fun()-> mnesia:read(Match) end).

search(Table, Values) ->
	Params = [ {P, V} || {P, {ok, V}} <- [ {I, maps:find(K, Values)} || {I, K} <- getFieldOffsets(Table)]],
	Pattern = erlang:make_tuple(length(Fields)+1, '_', [{1, Table}|Params]),
	mnesia:transaction(fun()-> mnesia:match_object(Pattern) end).

recordToMap(Table, Record) ->
	maps:from_list([ {K, element(I, Record)} || {I, K} <- getFieldOffsets(Table)]).

getFieldOffsets(Table) ->
	lists:zip(lists:seq(2, length(Fields)+1), getFields(Table)).

getFields(Table) ->	
	{fields, Fields} = proplists:lookup(fields, Table:module_info(attributes)),
	Fields.
