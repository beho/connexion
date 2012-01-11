% Complex tests of hierarchies of classes and properties related via domain/range construct. Ensures proper behaviour to related messages

-module (domainrange_tests).
-compile (export_all).

-include_lib("eunit/include/eunit.hrl").
-include ("test_helper.hrl").

generate_domainrange_test_() ->
	{inorder, {setup,
	 fun setup/0, fun teardown/1,
	[?_test( relationship() ),
	 ?_test( domain_members() ),
	 ?_test( range_members() )]
	}}.
	
	
setup() ->
	class:new( 'domain1' ),
	class:new( 'domain2' ),
	class:new( 'range' ),

	property:new( 'prop' ),
	
	property:new( 'supprop' ),
	class:new( 'supdomain' ),
	class:new( 'suprange' ),
	
	QID = make_ref(),
	
	'domain1' ! {add, "http://test.com/res-1", {self(), QID}},
	'domain2' ! {add, "http://test.com/res-2", {self(), QID}},
	'range' ! {add, "http://test.com/res-3", {self(), QID}},
	
	'prop' ! {add, {"http://test.com/subj-1", "http://test.com/obj-1"}, {self(), QID}},
	'prop' ! {add, {"http://test.com/subj-2", "http://test.com/obj-2"}, {self(), QID}},
	
	'supdomain' ! {add, "http://test.com/res-supdomain", {self(), QID}},
	'suprange' ! {add, "http://test.com/res-suprange", {self(), QID}},

	
	receive
		{QID, ok} -> true
	end,
	
	receive
		{QID, ok} -> true
	end,
	
	receive
		{QID, ok} -> true
	end,
	
	receive
		{QID, ok} -> true
	end,
	
	receive
		{QID, ok} -> true
	end,
	
	receive
		{QID, ok} -> true
	end,
	
	receive
		{QID, ok} -> true
	end,
	
	QID1 = make_ref(),
	'prop' ! {domain, 'domain1', {self(), QID1}},
	receive
		{QID1, ok} -> true
	end,
	
	QID2 = make_ref(),
	'prop' ! {domain, 'domain2', {self(), QID2}},
	receive
		{QID2, ok} -> true
	end,
	
	QID3 = make_ref(),
	'prop' ! {range, 'range', {self(), QID3}},
	receive
		{QID3, ok} -> true
	end,
	
	QID4 = make_ref(),
	'supprop' ! {domain, 'supdomain', {self(), QID4}},
	receive
		{QID4, ok} -> true
	end,
	
	QID5 = make_ref(),
	'supprop' ! {range, 'suprange', {self(), QID5}},
	receive
		{QID5, ok} -> true
	end,
	
	QID6 = make_ref(),
	'prop' ! {subPropertyOf, 'supprop', {self(), QID6}},
	receive
		{QID6, ok} -> true
	end.
	
	
teardown(_) ->
	'domain1' ! stop,
	'domain2' ! stop,
	'supdomain' ! stop,
	'suprange' ! stop,
	'range' ! stop,
	'prop' ! stop.
	
	
relationship() ->
	Domains = model:domainsOf( 'prop' ),
	?assertEqual( true, equalContent( Domains, ['domain1', 'domain2', 'supdomain'] ) ),
	
	Ranges = model:rangesOf( 'prop' ),
	?assertEqual( true, equalContent( Ranges, ['range', 'suprange'] ) ).
	
domain_members() ->
	Members = model:membersOf( 'domain1' ),
	?assertEqual( true, equalContent( Members, ["http://test.com/res-1", "http://test.com/subj-1", "http://test.com/subj-2"] ) ).
	
domain_supprop_members() ->
	Members = model:membersOf( 'supdomain' ),
	?assertEqual( true, equalContent( Members, ["http://test.com/res-supdomain", "http://test.com/subj-1", "http://test.com/subj-2"] ) ).
		
range_members() ->
	Members = model:membersOf( 'range' ),
	%io:format("~s~n", [Members]),
	?assertEqual( true, equalContent( Members, ["http://test.com/res-3", "http://test.com/obj-1", "http://test.com/obj-2"] ) ).	
	
range_supprop_members() ->
	Members = model:membersOf( 'suprange' ),
	%io:format("~s~n", [Members]),
	?assertEqual( true, equalContent( Members, ["http://test.com/res-suprange", "http://test.com/obj-1", "http://test.com/obj-2"] ) ).
	
	
