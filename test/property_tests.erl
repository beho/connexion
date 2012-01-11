% Tests of property module/actor. Ensures proper responses to isolated requests via messages.

-module (property_tests).
-compile (export_all).

-include_lib("eunit/include/eunit.hrl").
-include ("test_helper.hrl").


generate_property_test_() ->
	{inorder, {setup,
	 fun setup/0, fun teardown/1,
	[?_test(add()),
	 ?_test(explicitlyContains_true()),
	 ?_test(explicitlyContains_false()),
	 ?_test(contains_true()),
	 ?_test(contains_false()),
	 ?_test(explicitSize()),
	 ?_test(pairs()),
	 ?_test(explicitPairs()),
	 ?_test(objects_for()),
	 ?_test(explicit_objects_for()),
	 ?_test(subjects_for()),
	 ?_test(explicit_subjects_for()),
	 ?_test(domain_empty()),
	 ?_test(domain_add()),
	 ?_test(range_empty()),
	 ?_test(range_add()),
	 ?_test(subProperties_empty()),
	 ?_test(superProperties_empty()),
	 ?_test(simpleHierarchy())
	]
	}}.
	
setup() ->
	property:new(friendOf),
	class:new(domain),
	class:new(range1),
	property:new(super1).
	
teardown( _ ) ->
	friendOf ! stop,
	domain ! stop,
	range1 ! stop,
	super1 ! stop.

	
	
add() ->
	QID = make_ref(),
	friendOf ! {add, {"http://egg.com/red", "http://egg.com/green"}, {self(), QID}},
	receive
		Msg -> ?assertEqual( {QID,ok}, Msg )
	end,
	
	friendOf ! {add, {"http://egg.com/yellow", "http://egg.com/white"}, {self(), QID}},
	receive
		Msg1 -> ?assertEqual( {QID,ok}, Msg1 )
	end,
	
	friendOf ! {add, {"http://egg.com/yellow", "http://egg.com/black"}, {self(), QID}},
	receive
		Msg2 -> ?assertEqual( {QID,ok}, Msg2 )
	end.

explicitlyContains_true() ->
	QID =  make_ref(),
	friendOf ! {explicitlyContains, {"http://egg.com/red", "http://egg.com/green"}, {self(), QID}},
	receive
		Msg -> ?assertEqual( {QID, true}, Msg )
	end.

explicitlyContains_false() ->
	QID = make_ref(),
	friendOf ! {explicitlyContains, {"http://egg.com/red", "http://egg.com/yellow"}, {self(), QID}},
	receive
		Msg -> ?assertEqual( {QID, false}, Msg )
	end.
	
contains_true() ->
	QID = make_ref(),
	friendOf ! {contains, {"http://egg.com/red", "http://egg.com/green"}, {self(), QID}},
	receive
		Msg -> ?assertEqual( {QID, {true, 0}}, Msg )
	end.
	
contains_false() ->
	QID = make_ref(),
	friendOf ! {contains, {"http://egg.com/red", "http://egg.com/yellow"}, {self(), QID}},
	receive
		Msg -> ?assertEqual( {QID, {false, 0}}, Msg )
	end.

explicitSize() ->
	QID = make_ref(),
	friendOf ! {explicitSize, [], {self(), QID}},
	receive
		{QID, Size} -> ?assertEqual( 3, Size )
	end.
	
objects_for() ->
	QID = make_ref(),
	%List = interactor:objectsFor( friendOf, "http://egg.com/yellow" ),
	friendOf ! {objectsFor, "http://egg.com/yellow", {self(), QID}},
	receive
		{QID, {Objects, 0}} -> ?assertEqual( true, equalContent( Objects, ["http://egg.com/black", "http://egg.com/white"] ) )
	end.
	
explicit_objects_for() ->
	QID = make_ref(),
	%List = interactor:objectsFor( friendOf, "http://egg.com/yellow" ),
	friendOf ! {explicitObjectsFor, "http://egg.com/yellow", {self(), QID}},
	receive
		{QID, Objects} -> ?assertEqual( true, equalContent( Objects, ["http://egg.com/black", "http://egg.com/white"] ) )
	end.
	
subjects_for() ->
	QID = make_ref(),
	%List = interactor:subjectsFor( friendOf, "http://egg.com/black" ),
	friendOf ! {subjectsFor, "http://egg.com/black", {self(), QID}},
	receive
		{QID, {Subjects, 0}} -> ?assertEqual( ["http://egg.com/yellow"], Subjects )
	end.
	
explicit_subjects_for() ->
	QID = make_ref(),
	%List = interactor:subjectsFor( friendOf, "http://egg.com/black" ),
	friendOf ! {explicitSubjectsFor, "http://egg.com/black", {self(), QID}},
	receive
		{QID, Subjects} -> ?assertEqual( ["http://egg.com/yellow"], Subjects )
	end.

explicitPairs() ->
	QID = make_ref(),
	friendOf ! {explicitPairs, [], {self(), QID}},
	receive
		{QID, List} ->
			?assertEqual(true, equalContent(List, 
				[{"http://egg.com/red", "http://egg.com/green"}, {"http://egg.com/yellow", "http://egg.com/black"}, {"http://egg.com/yellow", "http://egg.com/white"}]))
	end.
	
pairs() ->
	QID = make_ref(),
	friendOf ! {pairs, [], {self(), QID}},
	receive
		{QID, {Pairs, 0}} -> ?assertEqual( true, equalContent(Pairs, 
			[{"http://egg.com/red", "http://egg.com/green"}, {"http://egg.com/yellow", "http://egg.com/black"}, {"http://egg.com/yellow", "http://egg.com/white"}]))
	end.	
	
domain_empty() ->
	QID = make_ref(),
	friendOf ! {getDomains, [], {self(), QID}},
	receive
		 % rdfs4a/b requires resource to be a domain of any property
		Msg -> ?assertEqual( {QID, {['http://www.w3.org/2000/01/rdf-schema#Resource'], 0}}, Msg )
	end.

domain_add() ->
	QID = make_ref(),
	friendOf ! {domain, domain, {self(), QID}},
	receive
		{QID, ok} ->
			friendOf ! {getDomains, [], {self(), QID}},
			receive
				% rdfs4a/b requires resource to be a domain of any property
				Msg -> ?assertEqual( {QID, {['http://www.w3.org/2000/01/rdf-schema#Resource', domain], 0}}, Msg )
			end
	end.

range_empty() ->
	QID = make_ref(),
	friendOf ! {getRanges, [], {self(), QID}},
	receive
		% rdfs4a/b requires resource to be a domain of any property
		Msg -> ?assertEqual( {QID, {['http://www.w3.org/2000/01/rdf-schema#Resource'], 0}}, Msg )
	end.

range_add() ->
	QID = make_ref(),

	friendOf ! {range1, range, {self(), QID}},
	receive
		{QID, ok} ->
			friendOf ! {getRanges, [], {self(), QID}},
			receive
				% rdfs4a/b requires resource to be a domain of any property
				Msg -> ?assertEqual( {QID, {['http://www.w3.org/2000/01/rdf-schema#Resource', range1], 0}}, Msg )
			end
	end.
	
subProperties_empty() ->
	QID = make_ref(),
	friendOf ! {subProperties, [], {self(), QID}},
	receive
		Msg -> ?assertEqual( {QID, {[friendOf], 0}}, Msg )
	end.
	
superProperties_empty() ->
	QID = make_ref(),
	friendOf ! {superProperties, [], {self(), QID}},
	receive
		Msg -> ?assertEqual( {QID, {[friendOf], 0}}, Msg )
	end.

simpleHierarchy() ->
	QID = make_ref(),
	friendOf ! {subPropertyOf, super1, {self(), QID}},
	receive
		{QID, ok} ->
			super ! {subProperties, [], {self(), QID}},
			receive
				Msg1 -> 
					?assertEqual( {QID, {[super1], 1}}, Msg1 ),
					receive
						Msg2 -> ?assertEqual( {QID, {[friendOf], 0}}, Msg2 )
					end
			end,
			
			friendOf ! {superProperties, [], {self(), QID}},
			receive
				Msg3 -> 
					?assertEqual( {QID, {[friendOf], 1}}, Msg3 ),
					receive
						Msg4 -> ?assertEqual( {QID, {[super1], 0}}, Msg4 )
					end
			end
			
	end.
	