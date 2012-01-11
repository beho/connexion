-module (plan_heap_tests).

-include_lib("eunit/include/eunit.hrl").
-include ("../include/querying.hrl").

generate_heap_test_() ->
	{inorder, {setup,
	 fun setup/0, fun teardown/1, 
	[?_test( empty() ),
	 ?_test( empty_after_insert() ),
	 ?_test( common_usage() )
	]
	}}.
	
setup() ->
	true.
	
teardown( _ ) ->
	true.
	


empty() ->
	A = plan_heap:new(),
	?assertEqual( [], A ),
	?assertEqual( true, plan_heap:isEmpty( A ) ).
	
empty_after_insert() ->
	?assertEqual( false, plan_heap:isEmpty( plan_heap:insert( #plan{ cost = 0 }, plan_heap:new() ) ) ).

common_usage() ->
	Plans = [#plan{cost = 5}, #plan{cost = 3}, #plan{cost = 2}, #plan{cost = 6}, #plan{cost = 7}],
	Queue = lists:foldl( fun(P, Q) -> plan_heap:insert(P, Q) end, plan_heap:new(), Plans ),
	Smallest = plan_heap:smallest( Queue ),
	?assertEqual( #plan{cost = 2}, Smallest ),
	{S, Queue1} = plan_heap:takeSmallest( Queue ),
	?assertEqual( Smallest, S ),
	?assertEqual( #plan{cost = 3}, plan_heap:smallest( Queue1 ) ).