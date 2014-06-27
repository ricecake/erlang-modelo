-module(modelo).

-export([search/2]).

%-fields([]).

search(Table, Values) ->
	Params = [ {P, V} || {P, {ok, V}} <- [ {I, maps:find(K, Values)} || {I, K} <- getFieldOffsets(Table)]],
	erlang:make_tuple(length(Fields)+1, '_', [{1, Table}|Params]).

recordToMap(Table, Record) ->
	maps:from_list([ {K, element(I, Record)} || {I, K} <- getFieldOffsets(Table)]).

getFieldOffsets(Table) ->
	lists:zip(lists:seq(2, length(Fields)+1), getFields(Table)).

getFields(Table) ->	
	{fields, Fields} = proplists:lookup(fields, Table:module_info(attributes)),
	Fields.
