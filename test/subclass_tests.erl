% Tests of hierarchy of classes. Ensures proper composite behaviour to related messages. Uses interactor !

-module (subclass_tests).
-compile (export_all).

-include_lib("eunit/include/eunit.hrl").
-include ("test_helper.hrl").

generate_subclass_test_() ->
	{inorder, {setup,
	 fun setup/0, fun teardown/1,
	[?_test( explicit_relationship() ),
	 ?_test( subClass() ),
	 ?_test( isSubClass() ),
	 ?_test( isExplicitSubClass() ),
	 ?_test( inclusion() ),
	 ?_test( contains() ),
	 ?_test( domainOf() ),
	 ?_test( rangeOf() )
	]
	}}.
	
setup() ->
	class:new('Agent'),
	class:new('Institution'),
	class:new('Event'),
	class:new('Organisation'),
	
	QID1 = make_ref(),
	'Institution' ! {subClassOf, 'Agent', {self(), QID1}},
	receive
		Msg -> ?assertEqual( {QID1, ok}, Msg )
	end,
	
	'Event' ! {subClassOf, 'Institution', {self(), QID1}},
	receive
		Msg1 -> ?assertEqual( {QID1, ok}, Msg1 )
	end,
	
	'Organisation' ! {subClassOf, 'Institution', {self(), QID1}},
	receive
		Msg2 -> ?assertEqual( {QID1, ok}, Msg2 )
	end,
	
	QID = make_ref(),
	'Agent' ! {add, "http://test.com/agent-1", {self(), QID}},
	'Institution' ! {add, "http://test.com/institution-1", {self(), QID}},
	'Event' ! {add, "http://test.com/event-1", {self(), QID}},
	'Organisation' ! {add, "http://test.com/organisation-1", {self(), QID}},
	receive {QID, ok} -> true end,
	receive {QID, ok} -> true end,
	receive {QID, ok} -> true end,
	receive {QID, ok} -> true end,
	
	property:new( prop ).

teardown( _ ) ->
	'Agent' ! stop,
	'Institution' ! stop,
	'Event' ! stop,
	'Organisation' ! stop,
	prop ! stop.


explicit_relationship() ->
	QID = make_ref(),
	'Institution' ! {explicitSubClasses, [], {self(), QID}},
	receive
		{QID, List} -> ?assertEqual( true, equalContent(['Event', 'Organisation'], List) )
	end.

subClass() ->
	SubClasses = model:subClassesOf( 'Agent' ),
	Res = equalContent( SubClasses, ['Agent', 'Institution', 'Event', 'Organisation'] ),
	io:format("~w~n", [SubClasses]),
	?assertEqual(true, Res).
	
isSubClass() ->
	?assertEqual( true, model:isSubClassOf( 'Organisation', 'Agent' ) ),
	?assertEqual( false, model:isSubClassOf( 'Agent', 'Organisation') ).

isExplicitSubClass() ->	
	?assertEqual( true, model:isExplicitSubClassOf( 'Institution', 'Agent' ) ),
	?assertEqual( false, model:isExplicitSubClassOf( 'Organisation', 'Agent' ) ).
	
inclusion() ->
	Members = model:membersOf( 'Agent' ),
	Res = equalContent( Members, ["http://test.com/agent-1", "http://test.com/institution-1",  "http://test.com/event-1", "http://test.com/organisation-1"] ),
	%io:format("~w~n~s~n", [Res, Members]),
	?assertEqual(true, Res).
	
contains() ->
	?assertEqual(true, model:containsMember( 'Agent', "http://test.com/organisation-1")),
	?assertEqual(false, model:containsMember( 'Event', "http://test.com/institution-1")).
	
domainOf() ->
	QID = make_ref(),
	'Agent' ! {domainOf, prop, {self(), QID}},
	receive
		{QID, ok} ->
			'Institution' ! {getDomainOf, [], {self(), QID}},
			receive
				{QID, {List, 1}} ->
					?assertEqual( [], List ),
					receive
						{QID, {List1, 0}} -> ?assertEqual( [prop], List1 )
					end
			end
	end.
	
rangeOf() ->
	QID = make_ref(),
	'Agent' ! {rangeOf, prop, {self(), QID}},
	receive
		{QID, ok} ->
			'Institution' ! {getRangeOf, [], {self(), QID}},
			receive
				{QID, {List, 1}} ->
					?assertEqual( [], List),
					receive
						{QID, {List1, 0}} -> ?assertEqual( [prop], List1 )
					end
			end
	end.