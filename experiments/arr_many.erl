-module(arr_many).

-compile([export_all]).

data1(Count, Size) ->
    %% size implies fixed-size array 
    %% but lets be explicit
    lists:map( fun(_) -> array:new([{size, Size}, {default, 0}, {fixed, true}]) end, lists:seq(1, Count) ).

data2(Count, Size) ->
    %% extensible array
    lists:map( fun(_) -> array:new([{size, Size}, {default, -1}, {fixed, false}]) end, lists:seq(1, Count) ).

data3(Count, Size) ->
    lists:map( fun(_) -> erlang:make_tuple(Size, 0) end, lists:seq(1, Count) ).

data4(Count, _) ->
    lists:map( fun(_) -> gb_trees:empty() end, lists:seq(1, Count) ).

array_set(ArrayList, I, Value) ->
    %% array indexing starts at 0
	
    lists:map( fun(Array) -> array:set(I - 1, Value, Array) end, ArrayList ).

tuple_set(TupleList, I, Value) ->
    %% tuple indexing starts at 1
    lists:map( fun(Tuple) -> setelement(I, Tuple, Value) end, TupleList).

tree_set(TreeList, I, Value) ->
    lists:map( fun(Tree) -> gb_trees:insert(I, Value, Tree) end, TreeList).

array_get(ArrayList, I) ->
    lists:map( fun(Array) -> array:get(I - 1, Array) end, ArrayList).

tuple_get(TupleList, I) ->
    lists:map( fun(Tuple) -> element(I, Tuple) end, TupleList).

tree_get(TreeList, I) ->
    lists:map( fun(Tree) -> gb_trees:get(I, Tree) end, TreeList).

get(_, _, 0) ->
    ok;

get(Fun, Data, Idx) ->
    Fun(Data, Idx),
    get(Fun, Data, Idx - 1).

set(_, Data, 0) ->
    Data;

set(Fun, Data, Idx) ->
    Data1 = Fun(Data, Idx, Idx),
    set(Fun, Data1, Idx - 1).

test() ->
    test(10000, 5).

test(Count, Size) ->
	{C1, DC1} = timer:tc(arr_many, data1, [Count, Size] ),
	{C2, DC2} = timer:tc(arr_many, data2, [Count, Size] ),
	{C3, DC3} = timer:tc(arr_many, data3, [Count, Size] ),
	{C4, DC4} = timer:tc(arr_many, data4, [Count, Size] ),
	
    %% fixed-size array
    {S1, D1} = timer:tc(arr_many, set, [{arr_many, array_set}, DC1, Size]),
    {G1, _} = timer:tc(arr_many, get, [{arr_many, array_get}, D1, Size]),
    %% extensible array
    {S2, D2} = timer:tc(arr_many, set, [{arr_many, array_set}, DC2, Size]),
    {G2, _} = timer:tc(arr_many, get, [{arr_many, array_get}, D2, Size]),
    %% tuple
    {S3, D3} = timer:tc(arr_many, set, [{arr_many, tuple_set}, DC3, Size]),
    {G3, _} = timer:tc(arr_many, get, [{arr_many, tuple_get}, D3, Size]),
    %% gb_trees
    {S4, D4} = timer:tc(arr_many, set, [{arr_many, tree_set}, DC4, Size]),
    {G4, _} = timer:tc(arr_many, get, [{arr_many, tree_get}, D4, Size]),
    %% results
    io:format("Fixed-size array: create: ~8w µs, get: ~8w µs, set: ~8w µs~n", [C1, G1 , S1]),
    io:format("Extensible array: create: ~8w µs, get: ~8w µs, set: ~8w µs~n", [C2, G2 , S2]),
    io:format("Tuple:            create: ~8w µs, get: ~8w µs, set: ~8w µs~n", [C3, G3 , S3]),
    io:format("Tree:             create: ~8w µs, get: ~8w µs, set: ~8w µs~n", [C4, G4 , S4]),
    ok.