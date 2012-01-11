% Tests of class module/actor. Ensures proper responses to isolated requests via messages. 

-module (class_tests).
-compile (export_all).

-include_lib("eunit/include/eunit.hrl").
-include ("test_helper.hrl").


generate_class_test_() ->
	{inorder, {setup,
	 fun setup/0, fun teardown/1,
	[?_test(add()),
	 ?_test(explicitlyContains_true()),
	 ?_test(explicitlyContains_false()),
	 ?_test(contains_true()),
	 ?_test(contains_false()),
%	 ?_test(explicitSize()),
	 ?_test(members()),
	 ?_test(explicitMembers()),
	 ?_test(domainOf_empty()),
	 ?_test(domainOf_add()),
	 ?_test(rangeOf_empty()),
	 ?_test(rangeOf_add()),
	 ?_test(subClasses_empty()),
	 ?_test(superClasses_empty()),
	 ?_test(simpleHierarchy())]
	}}.
	
setup() ->
	class:new(egg),
	class:new(super),
	property:new(dom),
	property:new(range).
	
teardown( _ ) ->
	egg ! stop,
	super ! stop,
	dom ! stop,
	range ! stop.
	
add() ->
	
	QID = make_ref(),
	
	egg ! {add, "http://egg.com/green", {self(), QID}},
	receive
		Msg1 -> ?assertEqual( {QID,ok}, Msg1 )
	end,
	
	egg ! {add, "http://egg.com/red", {self(), QID}},
	receive
		Msg -> ?assertEqual( {QID,ok}, Msg )
	end,
	
	egg ! {add, "http://egg.com/yellow", {self(), QID}},
	receive
		Msg2 -> ?assertEqual( {QID,ok}, Msg2 )
	end.

explicitlyContains_true() ->
	QID =  make_ref(),
	egg ! {explicitlyContains, "http://egg.com/green", {self(), QID}},
	receive
		{QID, Res} -> ?assertEqual( true, Res )
	end.
	
explicitlyContains_false() ->
	QID = make_ref(),
	egg ! {explicitlyContains, "http://egg.com/pink", {self(), QID}},
	receive
		{QID, Res1} -> ?assertEqual( false, Res1 )
	end.
	
contains_true() ->
	QID = make_ref(),
	egg ! {contains, "http://egg.com/green", {self(), QID}},
	receive
		{QID, Res} -> ?assertEqual( {true, 0}, Res )
	end.
	
contains_false() ->
	QID = make_ref(),
	egg ! {contains, "http://egg.com/pink", {self(), QID}},
	receive
		{QID, Res} -> ?assertEqual( {false, 0}, Res )
	end.
	
%explicitSize() ->
%	QID = make_ref(),
%	egg ! {explicitSize, [], {self(), QID}},
%	receive
%		{QID, Size} -> ?assertEqual( 3, Size )
%	end.
	
members() ->
	QID = make_ref(),
	egg ! {members, [], {self(), QID}},
	receive
		{QID, {Members, 0}} -> ?assertEqual(true, equalContent( Members, ["http://egg.com/green", "http://egg.com/red", "http://egg.com/yellow"]))
	end.

explicitMembers() ->
	QID = make_ref(),
	egg ! {explicitMembers, [], {self(), QID}},
	receive
		{QID, List} -> ?assertEqual(true, equalContent(List, ["http://egg.com/green", "http://egg.com/red", "http://egg.com/yellow"]) )
	end.
	
domainOf_empty() ->
	QID = make_ref(),
	egg ! {getDomainOf, [], {self(), QID}},
	receive
		Msg -> ?assertEqual( {QID, {[], 0}}, Msg )
	end.
	
domainOf_add() ->
	QID = make_ref(),
	
	egg ! {domainOf, dom, {self(), QID}},
	receive
		{QID, ok} ->
			egg ! {getDomainOf, [], {self(), QID}},
			receive
				Msg -> ?assertEqual( {QID, {[dom], 0}}, Msg)
			end
	end.
	
rangeOf_empty() ->
	QID = make_ref(),
	egg ! {getRangeOf, [], {self(), QID}},
	receive
		Msg -> ?assertEqual( {QID, {[], 0}}, Msg )
	end.
	
rangeOf_add() ->
	QID = make_ref(),
	egg ! {rangeOf, range, {self(), QID}},
	receive
		{QID, ok} ->
			egg ! {getRangeOf, [], {self(), QID}},
			receive
				Msg -> ?assertEqual( {QID, {[range], 0}}, Msg)
			end
	end.
	
subClasses_empty() ->
	QID = make_ref(),
	egg ! {subClasses, [], {self(), QID}},
	receive
		{QID, {SubClasses, 0}} -> ?assertEqual(['egg'], SubClasses)
	end.
	
superClasses_empty() ->
	QID = make_ref(),
	egg ! {superClasses, [], {self(), QID}},
	receive
		{QID, {SuperClasses, 0}} -> ?assertEqual(['egg'], SuperClasses)
	end.

simpleHierarchy() ->
	QID = make_ref(),
egg ! {subClassOf, super, {self(), QID}},
	receive
		{QID, ok} ->
			super ! {subClasses, [], {self(), QID}},
			receive
				Msg1 -> 
					?assertEqual( {QID, {[super], 1}}, Msg1 ),
					receive
						Msg2 -> ?assertEqual( {QID, {[egg], 0}}, Msg2 )
					end
			end,

			egg ! {superClasses, [], {self(), QID}},
			receive
				Msg3 -> 
					?assertEqual( {QID, {[egg], 1}}, Msg3 ),
					receive
						Msg4 -> ?assertEqual( {QID, {[super], 0}}, Msg4 )
					end
			end

	end.

	