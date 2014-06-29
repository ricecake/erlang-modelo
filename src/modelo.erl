-module(modelo).

-export([init/2, find/2, recordToMap/2, search/2]).

%-fields([]).

init(Table, Opts) -> 
	mnesia:create_table(Table, [{fields, getFields(Table)}|Opts]).

find(Table, Key) ->
	Match = erlang:make_tuple(2, [Table, Key]),
	mnesia:transaction(fun()-> mnesia:read(Match) end).

search(Table, Values) ->
	Params = [ {P, V} || {P, {ok, V}} <- [ {I, maps:find(K, Values)} || {I, K} <- getFieldOffsets(Table)]],
	Pattern = erlang:make_tuple(length(Params)+1, '_', [{1, Table}|Params]),
	mnesia:transaction(fun()-> mnesia:match_object(Pattern) end).

recordToMap(Table, Record) ->
	maps:from_list([ {K, element(I, Record)} || {I, K} <- getFieldOffsets(Table)]).

getFieldOffsets(Table) ->
	Fields = getFields(Table),
	lists:zip(lists:seq(2, length(Fields)+1), Fields).

getFields(Table) ->	
	{fields, Fields} = proplists:lookup(fields, Table:module_info(attributes)),
	Fields.
