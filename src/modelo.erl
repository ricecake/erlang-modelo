-module(modelo).

-export([init/2, find/2, grep/2, search/2, insert/2, update/3, recordToMap/2, mapToRecord/2]).

%-fields([]).

init(Table, Opts) -> 
	mnesia:create_table(Table, [{fields, getFields(Table)}|Opts]).

find(Table, Key) ->
	Match = erlang:make_tuple(2, [Table, Key]),
	[ recordToMap(Table, Rec) || Rec <- mnesia:transaction(fun()-> mnesia:read(Match) end)].

grep(Table, Values) ->
	Pattern = mapToRecord(Table, Values),
	[ recordToMap(Table, Rec) || Rec <- mnesia:transaction(fun()-> mnesia:match_object(Pattern) end) ].

search(Table, Pattern) -> [].

insert(Table, Row) ->
	Record = mapToRecord(Table, Row),
	[ recordToMap(Table, Rec) || Rec <- mnesia:transaction(fun()-> mnesia:write(Record) end)].

update(Table, Pattern, Update) ->
	BaseRecord = mapToRecord(Table, Update),
	Merge = fun DoUpdate(Base, _New, 0) -> Base;
	    	    DoUpdate(Base, New, Acc) -> 
			NewBase = case element(Acc, New) of
				'_' -> Base;
				Val -> setelement(Acc, Base, Val)
			end,
			DoUpdate(NewBase, New, Acc-1)
		end,
	Size = size(BaseRecord),
	mnesia:transaction(fun() -> 
		[ mnesia:write(Merge(OldRecord, BaseRecord, Size)) || OldRecord <- mnesia:match_object(mapToRecord(Table, Pattern))]
	end).
	
delete(Table, Key) -> mnesia:transaction(fun()-> mnesia:delete({Table, Key}) end);
delete(Table, Values)-> 
	Pattern = mapToRecord(Table, Values),
	[ delete(Table, element(2, Rec)) || Rec <- mnesia:transaction(fun()-> mnesia:match_object(Pattern) end) ].

join(Table, Relationship, Record) ->
	resolveJoin(lists:keyfind(Relationship, 2, getJoins(Table)), Table, Record).


resolveJoin({belongs_to, Remote, Field}, Table, Record) -> ok;
resolveJoin({has_many,   Remote, Field}, Table, Record) -> ok.

mapToRecord(Table, Values) -> 
	Params = [ {P, V} || {P, {ok, V}} <- [ {I, maps:find(K, Values)} || {I, K} <- getFieldOffsets(Table)]],
	erlang:make_tuple(length(Params)+1, '_', [{1, Table}|Params]).

recordToMap(Table, Record) ->
	maps:from_list([ {K, element(I, Record)} || {I, K} <- getFieldOffsets(Table)]).

getFieldOffsets(Table) ->
	Fields = getFields(Table),
	lists:zip(lists:seq(2, length(Fields)+1), Fields).

getFields(Table) -> lookupAttr(Table, fields).
getJoins(Table) -> lookupAttr(Table, join).

lookupAttr(Module, Attr) ->
	{Attr, Data} = proplists:lookup(Attr, Module:module_info(attributes)),
	Data.

