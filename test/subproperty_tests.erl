% Tests of hierarchy of properties. Ensures proper composite behaviour to related messages. Uses interactor !

-module (subproperty_tests).
-compile (export_all).

-include_lib("eunit/include/eunit.hrl").
-include ("test_helper.hrl").

generate_subproperty_test_() ->
	{inorder, {setup,
	 fun setup/0, fun teardown/1,
	[?_test( explicit_relationship() ),
	 ?_test( subProperty() ),
	 ?_test( isSubProperty() ),
	 ?_test( isExplicitSubProperty() ),
	 ?_test( inclusion() ),
	 ?_test( contains() )
	]
	}}.
	
setup() ->
	property:new('involvedIn'),
	property:new('attendedTo'),
	property:new('participatedOn'),
	property:new('inCommitteeOf'),
		
	QID1 = make_ref(),
	'attendedTo' ! {subPropertyOf, 'involvedIn', {self(), QID1}},
	receive
		Msg -> ?assertEqual( {QID1, ok}, Msg )
	end,
	
	'participatedOn' ! {subPropertyOf, 'involvedIn', {self(), QID1}},
	receive
		Msg1 -> ?assertEqual( {QID1, ok}, Msg1 )
	end,
	
	'inCommitteeOf' ! {subPropertyOf, 'attendedTo', {self(), QID1}},
	receive
		Msg2 -> ?assertEqual( {QID1, ok}, Msg2 )
	end,
	
	QID = make_ref(),
	'attendedTo' ! {add, {"http://test.com/agent-1", "http://test.com/event-1"}, {self(), QID}},
	'participatedOn' ! {add, {"http://test.com/agent-1", "http://test.com/project-1"}, {self(), QID}},
	
	receive {QID, ok} -> true end,
	receive {QID, ok} -> true end.
	
	

teardown( _ ) ->
	'involvedIn' ! stop,
	'attendedTo' ! stop,
	'participatedOn' ! stop.


explicit_relationship() ->
	QID = make_ref(),
	'involvedIn' ! {explicitSubProperties, [], {self(), QID}},
	receive
		{QID, List} -> ?assertEqual( true, equalContent(['attendedTo', 'participatedOn'], List) )
	end.

subProperty() ->
	SubProperties = model:subPropertiesOf( 'involvedIn' ),
	io:format("~w~n", [SubProperties]),
	?assertEqual(true, equalContent( SubProperties, ['involvedIn', 'participatedOn', 'attendedTo', 'inCommitteeOf'])).
	
isSubProperty() ->
	?assertEqual(true, model:isSubPropertyOf( 'inCommitteeOf', 'involvedIn' ) ),
	?assertEqual(false, model:isSubPropertyOf( 'involvedIn', 'inCommitteeOf' ) ).
	
isExplicitSubProperty() ->
	?assertEqual(true, model:isExplicitSubPropertyOf( 'attendedTo', 'involvedIn' ) ),
	?assertEqual(false, model:isExplicitSubPropertyOf( 'inCommitteeOf', 'involvedIn' ) ).
	
inclusion() ->
	Pairs = model:pairsFor( 'involvedIn' ),
	?assertEqual(true, equalContent( Pairs, [{"http://test.com/agent-1", "http://test.com/event-1"}, {"http://test.com/agent-1", "http://test.com/project-1"}])).
	
contains() ->
	?assertEqual(true, model:containsPair( 'involvedIn', {"http://test.com/agent-1", "http://test.com/event-1"})),
	?assertEqual(false, model:containsPair( 'inCommitteeOf', {"http://test.com/agent-1", "http://test.com/event-1"})).