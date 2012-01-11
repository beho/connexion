-module (model_tests).

-compile (export_all).

-include_lib ("eunit/include/eunit.hrl").
-include ("test_helper.hrl").

generate_property_test_() ->
	{inorder, {setup,
	 fun setup/0, fun teardown/1, 
	[?_test( foldResponses() ),
	 ?_test( foldResponsesWithConfirmation() )
	]
	}}.
	
setup() ->
	true.

teardown(_) ->
	true.
	
foldResponses() ->
	QID = make_ref(),
	List = [{[7, 2, 3], 1}, {[4], 2}, {[5, 6], 0}, {[], 1}, {[7, 8], 0}],
	
	Folded = lists:foldl( 
		fun({L, _ToCome} = E, Acc) -> self() ! {QID, E}, ordsets:union( L, Acc ) end, ordsets:new(), List),
	
	FoldedResponses = model:foldSetResponses( QID ),
	io:format("~w~n~w~n", [Folded, FoldedResponses]),
	?assertEqual( true, equalContent( Folded, FoldedResponses ) ).
	
foldResponsesWithConfirmation() ->
	true.