-module(modelo).

-export([search/2]).

%-fields([]).

search(Table, Values) ->
	{fields, Fields} = proplists:lookup(fields, Table:module_info(attributes)),
	Params = [ {P, V} || {P, {ok, V}} <- [ {I, maps:find(K, Values)} || {I, K} <- lists:zip(lists:seq(2, length(Fields)+1), Fields)]],
	erlang:make_tuple(length(Fields)+1, '_', [{1, Table}|Params]).
