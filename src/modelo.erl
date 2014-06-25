-module(modelo).

-export([search/2]).

%-fields([]).

search(Table, Values) ->
	{fields, Fields} = proplists:lookup(fields, Table:module_info(attributes)),
	lists:zip(lists:seq(1, length(Fields)+1), [Table | Fields]).
