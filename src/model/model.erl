-module (model).

-compile (export_all).
-export ([
	initialize/0,

	addMember/2,
	subClassOf/2,
	
	addPair/2,
	subPropertyOf/2,
	domain/2,
	range/2,
%	ensureExistenceOfClass/1,
%	ensureExistenceOfProperty/1,
	
	membersOf/1,
	containsMember/2,
	subClassesOf/1,
	explicitSubClassesOf/1,
	explicitSuperClassesOf/1,
	isSubClassOf/2,
	isExplicitSubClassOf/2,

	subPropertiesOf/1,
	isSubPropertyOf/2,
	isExplicitSubPropertyOf/2,
	explicitSuperPropertiesOf/1,
	domainsOf/1,
	isDomainOf/2,
	rangesOf/1,
	isRangeOf/2,
	pairsFor/1,
	containsPair/2,
	subjectsFor/2,
	objectsFor/2,
	
	statistics/0,
	
	sendMessageAndFoldResponses/6, sendMessageAndFoldSetResponses/3,
	foldResponses/5, foldSetResponses/1,
	foldResponsesWithConfirmation/3, foldResponsesWithConfirmation/5,
%	concatenation/0,
	
	isKnownClass/1, isKnownProperty/1
	
	%foldEmptySet/0, foldUnion/2, foldSetToList/1
]).

-include ("messaging.hrl").
-include ("querying.hrl").
-include ("stats.hrl").

-include ("vocabularies/rdf.hrl").
-include ("vocabularies/rdfs.hrl").

initialize() ->
	class:new( list_to_atom( ?rdfs?Class ) ),
	class:new( list_to_atom( ?rdf?Property ) ).

%% ========== Adding ==========
%% === Class ===
addMember( Class, Url ) when is_list( Class ) ->
	addMember( list_to_atom( Class ), Url );
addMember( Class, Url ) when is_atom( Url ) ->
	addMember( Class, atom_to_list( Url ) );
addMember( Class, Url ) when is_atom( Class ) ->
	ensureExistenceOfClass( Class ),
	QID = make_ref(),
	Class ! {add, Url, {self(), QID}},
	receive
		{QID, ok} -> true
	end.

subClassOf( Class, SuperClass ) when is_list( Class ) ->
	subClassOf( list_to_atom( Class ), SuperClass );
subClassOf( Class, SuperClass ) when is_list( SuperClass ) ->
	subClassOf( Class, list_to_atom( SuperClass ) );
subClassOf( Class, SuperClass ) when is_atom( Class ) and is_atom( SuperClass )->
	ensureExistenceOfClass( Class ),
	ensureExistenceOfClass( SuperClass ),
	QID = make_ref(),
	Class ! {subClassOf, SuperClass, {self(), QID}},
	receive
		{QID, ok} -> true
	end.
	
%% === Property ===

addPair( Property, Pair ) when is_list( Property ) ->
	addPair( list_to_atom( Property ), Pair );
addPair( Property, {_S, _O} = Pair ) when is_atom( Property ) ->
	ensureExistenceOfProperty( Property ),
	QID = make_ref(),
	Property ! {add, Pair, {self(), QID}},
	receive
		{QID, ok} -> true
	end.

subPropertyOf( Property, SuperProperty ) when is_list( Property ) ->
	subPropertyOf( list_to_atom( Property ), SuperProperty );
subPropertyOf( Property, SuperProperty ) when is_list( SuperProperty ) ->
	subPropertyOf( Property, list_to_atom( SuperProperty ) );
subPropertyOf( Property, SuperProperty ) when is_atom( Property) and is_atom( SuperProperty ) ->
	ensureExistenceOfProperty( Property ),
	ensureExistenceOfProperty( SuperProperty ),
	QID = make_ref(),
	Property ! {subPropertyOf, SuperProperty, {self(), QID}},
	receive
		{QID, ok} -> true
	end.	

domain( Property, Class ) when is_list( Property ) ->
	domain( list_to_atom( Property ), Class );
domain( Property, Class ) when is_list( Class ) ->
	domain( Property, list_to_atom( Class ) );
domain( Property, Class ) when is_atom( Property ) and is_atom( Class ) ->
	ensureExistenceOfProperty( Property ),
	ensureExistenceOfClass( Class ),
	QID = make_ref(),
	Property ! {domain, Class, {self(), QID}},
	receive
		{QID, ok} -> true
	end.

range( Property, Class ) when is_list( Property ) ->
	range( list_to_atom( Property ), Class );
range( Property, Class ) when is_list( Class ) ->
	range( Property, list_to_atom( Class ) );
range( Property, Class ) when is_atom( Property ) and is_atom( Class ) ->
	ensureExistenceOfProperty( Property ),
	ensureExistenceOfClass( Class ),
	QID = make_ref(),
	Property ! {range, Class, {self(), QID}},
	receive
		{QID, ok} -> true
	end.
	
%% ========== Querying ==========

%% === Class ===

membersOf( Class ) when is_atom( Class ) ->
	sendMessageAndFoldSetResponses( Class, members, [] );
membersOf( Class ) ->
	forKnownClass( Class, fun membersOf/1, [], [] ).
		
membersOfAsPairs( Class ) when is_atom( Class ) ->
	sendMessageAndFoldSetResponses( Class, members, atom_to_list( Class ) );
membersOfAsPairs( Class ) ->
	forKnownClass( Class, fun membersOfAsPairs/1, [], [] ).
		
classes() ->
	membersOf( ?'rdfs:Class' ).

properties() ->
	membersOf( ?'rdf:Property' ).
	
	

subClassesOf( Class ) when is_atom( Class ) ->
	sendMessageAndFoldSetResponses( Class, subClasses, [] );
subClassesOf( Class ) ->
	forKnownClass( Class, fun subClassesOf/1, [], [] ).
		
subClassesOfAsPairs( Class ) when is_atom( Class ) ->
	sendMessageAndFoldSetResponses( Class, subClasses, atom_to_list( Class ) );
subClassesOfAsPairs( Class ) ->
	forKnownClass( Class, fun subClassesOfAsPairs/1, [], [] ).



explicitSubClassesOf( Class ) when is_atom( Class ) ->
	QID = make_ref(),
	Class ! {explicitSubClasses, [], {self(), QID} },
	receive
		{QID, List} -> List
	end;
explicitSubClassesOf( Class ) ->
	forKnownClass( Class, fun explicitSubClassesOf/1, [], [] ).
		
explicitSuperClassesOf( Class ) when is_atom( Class ) ->
	QID = make_ref(),
	Class ! {explicitSuperClasses, [], {self(), QID}},
	receive
		{QID, List} -> List
	end;
explicitSuperClassesOf( Class ) ->
	forKnownClass( Class, fun explicitSuperClassesOf/1, [], [] ).
	

isSubClassOf( Class, SuperClass ) when is_atom( Class ) and is_atom( SuperClass ) ->
	sendMessageAndFoldResponses( SuperClass, isSubClass, Class, fun foldOr/2, fun identity/1, false );
isSubClassOf( Class, SuperClass ) ->
	forKnownClasses( [Class, SuperClass], fun isSubClassOf/2, [], false ).
		


isExplicitSubClassOf( Class, SuperClass ) when is_atom( Class ) and is_atom( SuperClass ) ->
	QID = make_ref(),
	SuperClass ! {isExplicitSubClass, Class, {self(), QID}},
	receive
		{QID, Contains} -> Contains
	end;
isExplicitSubClassOf( Class, SuperClass ) ->
	forKnownClasses( [Class, SuperClass], fun isExplicitSubClassOf/2, [], false ).
	
	
	
containsMember( Class, Url ) when is_atom( Class ) ->
	sendMessageAndFoldResponses( Class, contains, Url, fun foldOr/2, fun identity/1, false );
containsMember( Class, Url ) ->
	forKnownClass( Class, fun containsMember/2, [Url], false ).



explicitlyContainsMember( Class, Url ) when is_atom( Class )->
	QID = make_ref(),
	Class ! {explicitlyContains, to_list( Url ), {self(), QID}},
	receive
		{QID, Value} -> Value
	end;
explicitlyContainsMember( Class, Url ) ->
	forKnownClass( Class, fun explicitlyContainsMember/2, [Url], false ).
	
%% === Property ===

subPropertiesOf( Property ) when is_atom( Property ) ->
	sendMessageAndFoldSetResponses( Property, subProperties, [] );
subPropertiesOf( Property ) ->
	forKnownProperty( Property, fun subPropertiesOf/1, [], [] ).
		
subPropertiesOfAsPairs( Property ) when is_atom( Property ) ->
	sendMessageAndFoldSetResponses( Property, subProperties, atom_to_list( Property ) );
subPropertiesOfAsPairs( Property ) ->
	forKnownProperty( Property, fun subPropertiesOfAsPairs/1, [], [] ).
	
	
isSubPropertyOf( Property, SuperProperty ) when is_atom( Property ) and is_atom( SuperProperty ) ->
	sendMessageAndFoldResponses( SuperProperty, isSubProperty, Property, fun foldOr/2, fun identity/1, false );
isSubPropertyOf( Property, SuperProperty ) ->
	forKnownProperties( [Property, SuperProperty], fun isSubPropertyOf/2, [], false ).
		


isExplicitSubPropertyOf( Property, SuperProperty ) when is_atom( Property ) and is_atom( SuperProperty ) ->
	QID = make_ref(),
	SuperProperty ! {isExplicitSubProperty, Property, {self(), QID}},
	receive
		{QID, Contains} -> Contains
	end;
isExplicitSubPropertyOf( Property, SuperProperty ) ->
	forKnownProperties( [Property, SuperProperty], fun isExplicitSubPropertyOf/2, [], false ).
		
explicitSuperPropertiesOf( Property ) when is_atom( Property ) ->
	QID = make_ref(),
	Property ! {explicitSuperProperties, [], {self(), QID}},
	receive
		{QID, SuperProperties} -> SuperProperties
	end;
explicitSuperPropertiesOf( Property ) ->
	forKnownProperty( Property, fun explicitSuperPropertiesOf/1, [], [] ).


domainsOf( Property ) when is_atom( Property )->
	sendMessageAndFoldSetResponses( Property, getDomains, [] );
domainsOf( Property ) ->
	forKnownProperty( Property, fun domainsOf/1, [], [] ).

domainsOfAsPairs( Property ) when is_atom( Property )->
	sendMessageAndFoldSetResponses( Property, getDomains, atom_to_list( Property ) );
domainsOfAsPairs( Property ) ->
	forKnownProperty( Property, fun domainsOfAsPairs/1, [], [] ).

	


isDomainOf( Class, Property ) when is_atom( Class ) and is_atom( Property ) ->
	sendMessageAndFoldResponses( Property, isDomain, Class, fun foldOr/2, fun identity/1, false );
isDomainOf( Class, Property ) ->
	forKnownSetsOfTypes( [Class, Property], [fun isKnownClass/1, fun isKnownProperty/1], fun isDomainOf/2, [], false ).



rangesOf( Property ) when is_atom( Property )->
	sendMessageAndFoldSetResponses( Property, getRanges, [] );
rangesOf( Property ) ->
	forKnownProperty( Property, fun rangesOf/1, [], [] ).
	

rangesOfAsPairs( Property ) when is_atom( Property )->
	sendMessageAndFoldSetResponses( Property, getRanges, atom_to_list( Property ) );
rangesOfAsPairs( Property ) ->
	forKnownProperty( Property, fun rangesOfAsPairs/1, [], [] ).



isRangeOf( Class, Property ) when is_atom( Class ) and is_atom( Property ) ->
	sendMessageAndFoldResponses( Property, isRange, Class, fun foldOr/2, fun identity/1, false );
isRangeOf( Class, Property ) ->
	forKnownSetsOfTypes( [Class, Property], [fun isKnownClass/1, fun isKnownProperty/1], fun isRangeOf/2, [], false ).



pairsFor( Property ) when is_atom( Property )  ->
	sendMessageAndFoldSetResponses( Property, pairs, [] );
pairsFor( Property ) ->
	forKnownProperty( Property, fun pairsFor/1, [], [] ).



containsPair( Property, Pair ) when is_atom( Property ) ->
	sendMessageAndFoldResponses( Property, contains, Pair, fun foldOr/2, fun identity/1, false );
containsPair( Property, Pair ) ->
	forKnownProperty( Property, fun containsPair/2, [Pair], false ).
	
	
	
subjectsFor( Property, Object ) when is_atom( Property ) ->
	sendMessageAndFoldSetResponses( Property, subjectsFor, Object );
subjectsFor( Property, Object ) ->
	forKnownProperty( Property, fun subjectsFor/2, [Object], [] ).


subjectsForAsPairs( Property, Object ) when is_atom( Property ) ->
	sendMessageAndFoldSetResponses( Property, subjectsForAsPairs, {Object, atom_to_list( Property )} );
subjectsForAsPairs( Property, Object ) ->
	forKnownProperty( Property, fun subjectsForAsPairs/2, [Object], [] ).


		
explicitSubjectsFor( Property, Object ) when is_atom( Property ) ->
	QID = make_ref(),
	Property ! {explicitSubjectsFor, Object, {self(), QID}},
	receive
		{QID, Subjects} -> Subjects
	end;
explicitSubjectsFor( Property, Object ) ->
	forKnownProperty( Property, fun explicitSubjectsFor/2, [Object], [] ).



objectsFor( Property, Subject ) when is_atom( Property ) ->
	sendMessageAndFoldSetResponses( Property, objectsFor, Subject );
objectsFor( Property, Subject ) ->
	forKnownProperty( Property, fun objectsFor/2, [Subject], [] ).
		

objectsForAsPairs( Property, Subject ) when is_atom( Property ) ->
	sendMessageAndFoldSetResponses( Property, objectsFor, {Subject, atom_to_list( Property )} );
objectsForAsPairs( Property, Subject ) ->
	forKnownProperty( Property, fun objectsForAsPairs/2, [Subject], [] ).
		

explicitObjectsFor( Property, Subject ) when is_atom( Property ) ->
	QID = make_ref(),
	Property ! {explicitObjectsFor, Subject, {self(), QID}},
	receive
		{QID, Objects} -> Objects
	end;
explicitObjectsFor( Property, Subject ) ->
	forKnownProperty( Property, fun explicitObjectsFor/2, [Subject], [] ).
		
%% ========== STATISTICS =======

	
statistics() ->
	Properties1 = statsForProperties(),
	{Properties2,Classes} = statsForClasses(),

	Properties = dict:merge( fun statsDictMerge/3, Properties2, Properties1 ),
%	io:format( "P1: ~p~n~nP2: ~p~n~nP: ~p~n", [Properties1, Properties2, Properties]),

	{PropertyTriples, AvgObjectOccurence} = dict:fold( 
		fun(_, #property_stats{ pairsCount = PC, avgObjectOccurence = AOO}, {Sum, Avg}) -> 
			{PC + Sum, avgOccurrence( AOO, Avg )} end, {0, 0}, Properties ),
	
	TriplesCount = PropertyTriples +
		dict:fold( fun(_, V, Acc) -> V#class_stats.membersCount + Acc end, 0, Classes ),
		
	#stats{ 
		triplesCount = TriplesCount,
		avgObjectOccurence = AvgObjectOccurence,
		properties = Properties,
		classes = Classes }.

statsForProperties() ->
	RootProperties = lists:filter( fun(P) -> explicitSuperPropertiesOf( P ) =:= [] end, model:properties() ),
%	io:format("root properties: ~p~n", [RootProperties] ),
	
	% make each root compute statistics for its tree
	QIDs = lists:map( 
		fun(P) -> 
			QID = make_ref(), 
			list_to_atom(P) ! {computeStatistics, [], {self(), QID}},
			QID
		end, RootProperties ),
		
	%Stats = 
	lists:foldl( 
		fun(QID, Acc) ->
			receive {QID, {{_, Dict}, 0}} -> 
				dict:merge( fun statsDictMerge/3, Dict, Acc )
			end 
		end, dict:new(), QIDs ).
		
%	io:format( "~p ~n", [dict:to_list(Stats)]),
%	Stats.
	
statsForClasses() ->
	RootClasses = lists:filter( fun(C) -> explicitSuperClassesOf( C ) =:= [] end, model:classes() ),
	%RootClasses = [?'rdfs:Resource']
%	io:format( "root classes: ~p~n", [RootClasses] ),
	
	QIDs = lists:map(
		fun(C) ->
			QID = make_ref(),
			list_to_atom(C) ! {computeStatistics, [], {self(), QID}},
			QID
		end, RootClasses ),
		
	%{ClassStats, SubClassOfStat} = 
	lists:foldl(
		fun(QID, {TotalPropertyDict, TotalClassDict}) ->
			receive {QID, {{_, PropertyDict, ClassDict}, 0}} ->
				{dict:merge( fun statsDictMerge/3, PropertyDict, TotalPropertyDict),
				 dict:merge( fun statsDictMerge/3, ClassDict, TotalClassDict )}
				
			end
		end, {dict:new(), dict:new()}, QIDs	).
		
%	io:format( "~p~n~p~n", [ClassStats, SubClassOfStat] ),
%	ClassStats.
	
	
	
%% ========== PRIVATE ==========

ensureExistenceOfClass( ClassName ) when is_list( ClassName ) ->
	ensureExistenceOfClass( list_to_atom( ClassName ) );
ensureExistenceOfClass( ClassName ) when is_atom( ClassName ) ->
	 case whereis( ClassName ) of
		undefined ->
			class:new( ClassName ),
			true;
		_ -> explicitlyContainsMember( ?rdfs?Class, ClassName )
	end.



ensureExistenceOfProperty( PropertyName ) when is_list( PropertyName ) ->
	ensureExistenceOfProperty( list_to_atom( PropertyName ) );
ensureExistenceOfProperty( PropertyName ) when is_atom( PropertyName ) ->
	case whereis( PropertyName ) of
		undefined ->
			property:new( PropertyName ),
			true;
		_ -> explicitlyContainsMember( ?rdf?Property, PropertyName )
	end.
	
	
	
isKnownClassOfType( ClassUrl, TypeClassUrl ) when is_list( TypeClassUrl ) ->
	isKnownClassOfType( ClassUrl, list_to_atom( TypeClassUrl ) );
isKnownClassOfType( ClassUrl, TypeClassUrl ) when is_list( ClassUrl ) ->
	% must avoid creation of useless atoms for erlang's table is not GCed and
	% its size is limited
	try list_to_existing_atom( ClassUrl ) of
		UrlAtom -> isKnownClassOfType( UrlAtom, TypeClassUrl )
	catch
		error:badarg -> false
	end;
isKnownClassOfType( ClassUrl, TypeClassUrl ) ->
	isKnownClassOfType( atom_to_list( ClassUrl ), ClassUrl, TypeClassUrl ).


isKnownClassOfType( ClassUrl, ClassUrlAtom, TypeClassUrl ) ->
	case whereis( ClassUrlAtom ) of
		undefined -> false;
		_ -> explicitlyContainsMember( TypeClassUrl, ClassUrl )
	end.		


isKnownClass( Url ) -> isKnownClassOfType( Url, ?'rdfs:Class').
isKnownProperty( Url ) -> isKnownClassOfType( Url, ?'rdf:Property' ).




	
	

applyWithToAtomConversion( Fun, Sets, Args ) ->
	apply( Fun, lists:map( fun to_atom/1, Sets ) ++ Args ).



forKnownSetOfType( Set, TypePredicate, Fun, Args, ElseVal ) ->
	case TypePredicate( Set ) of
		true -> 
			apply( Fun, [to_atom( Set )|Args] );
		false -> ElseVal
	end.

forKnownSetsOfType( Sets, TypePredicate, Fun, Args, ElseVal ) ->
	AllClasses = lists:foldr( fun(S, Acc) -> TypePredicate( S ) and Acc end, true, Sets ),
	case AllClasses of
		true -> applyWithToAtomConversion( Fun, Sets, Args );
		false -> ElseVal
	end.

forKnownSetsOfTypes( Sets, TypePredicateList, Fun, Args, ElseVal ) ->
	AllConform = lists:foldr( fun foldOr/2, true, 
		lists:zipwith( fun(S, Predicate) -> Predicate( S ) end, Sets, TypePredicateList ) ),
	case AllConform of
		true -> applyWithToAtomConversion( Fun, Sets, Args );
		false -> ElseVal
	end.



forKnownClass( Class, Fun, Args, ElseVal ) ->
	forKnownSetOfType( Class, fun isKnownClass/1, Fun, Args, ElseVal ).

forKnownClasses( List, Fun, Args, ElseVal ) ->
	forKnownSetsOfType( List, fun isKnownClass/1, Fun, Args, ElseVal ).



forKnownProperty( Property, Fun, Args, ElseVal ) ->
	forKnownSetOfType( Property, fun isKnownProperty/1, Fun, Args, ElseVal ).

forKnownProperties( List, Fun, Args, ElseVal ) ->
	forKnownSetsOfType( List, fun isKnownProperty/1, Fun, Args, ElseVal ).
	