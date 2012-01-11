-module (property).

-include ("actor.hrl").

-include ("stats.hrl").
-include ("vocabularies/rdf.hrl").
-include ("vocabularies/rdfs.hrl").


-record (property, {
	url,
	pairs, reversed,
	subProperties = sets:new(), superProperties = sets:new(), 
	% rdfs4a/b entailment rule
	domains = sets:from_list( [?'rdfs:Resource'] ), ranges = sets:from_list( [?'rdfs:Resource'] )
	}).

%% Actor Methods

initialize( Url, _ ) ->
	Pairs = ets:new( Url, [bag] ),
	Reversed = ets:new( list_to_atom(atom_to_list( Url ) ++ "_"), [bag] ),
	
	UrlAsList = atom_to_list( Url ),
	
	model:addMember( ?rdf?Property, UrlAsList ),
	
	% rdfs4a	uuu aaa xxx .	uuu rdf:type rdfs:Resource .
	% rdfs4b	uuu aaa vvv .	vvv rdf:type rdfs:Resource .
	model:ensureExistenceOfClass( ?'rdfs:Resource' ),
	QID = make_ref(),
	?'rdfs:Resource' ! {domainOf, Url, {self(), QID}},
	receive
		{QID, ok} ->
			?'rdfs:Resource' ! {rangeOf, Url, {self(), QID}},
			receive
				{QID, ok} ->
					#property{ url = UrlAsList, pairs = Pairs, reversed = Reversed }
			end
	end.

	
stop( #property{ pairs = Pairs, reversed = Reversed } ) ->
	ets:delete( Pairs ),
	ets:delete( Reversed ),
	ok.
	
	
methodFor( Selector ) ->
	case Selector of
		?M( subPropertyOf );
		?M( subProperties );
		?M( explicitSubProperties );		
		?M( isSubProperty );
		?M( isExplicitSubProperty );
		
		?M( superPropertyOf );
		?M( superProperties );
		?M( explicitSuperProperties );
		
		?M( domain );
		?M( getDomains );
		?M( isDomain );
		
		?M( range );
		?M( getRanges );
		?M( isRange );
		
		?M( add );
		
		?M( contains );
		?M( explicitlyContains );
		
		?M( pairs );
		?M( explicitPairs );
		
		?M( subjects );
		?M( subjectsFor );
		?M( subjectsForAsPairs );
		?M( explicitSubjectsFor );
		
		?M( objects );
		?M( objectsFor );
		?M( objectsForAsPairs );
		?M( explicitObjectsFor );
		
%		?M( size );
		?M( explicitSize );
		
		?M( computeStatistics );
	
		?DNU_HANDLER
	end.

%% Handling of structure related messages

subPropertyOf( Property, {Receiver, QID}, #property{ url = Self, superProperties = SuperProperties } = PropertyInfo) ->
	NewSuperProperties = set_actor_helper:subSetOf( Self, Property, SuperProperties, superPropertyOf, QID ),
	Receiver ! {QID, ok},
	{ok, PropertyInfo#property{ superProperties = NewSuperProperties } }.
	
superPropertyOf( Property, {Receiver, QID}, #property{ subProperties = SubProperties } = PropertyInfo ) ->
	NewSubProperties = set_actor_helper:superSetOf( Property, SubProperties ),
	Receiver ! {QID, ok},
	{ok, PropertyInfo#property{ subProperties = NewSubProperties } }.


subProperties( RespondingProperty, {Receiver, QID} = ID, #property{ url = Self, subProperties = SubProperties } = PropertyInfo ) ->
%	io:format("~p~n", [RespondingProperty]),
	Response = case RespondingProperty of
		[] -> [Self];
		_ -> [{RespondingProperty, Self}]
	end,
	Receiver ! {QID, {Response, sets:size( SubProperties )}},
	sendToAll( SubProperties, subProperties, RespondingProperty, ID),
	{ok, PropertyInfo}.
	
superProperties( _, {Receiver, QID} = ID, #property{ url = Self, superProperties = SuperProperties } = PropertyInfo ) ->
	Receiver ! {QID, {[Self], sets:size( SuperProperties )}},
	sendToAll( SuperProperties, superProperties, [], ID),
	{ok, PropertyInfo}.


explicitSubProperties( _, {Receiver, QID}, #property{ subProperties = SubProperties } = PropertyInfo ) ->
	Receiver ! {QID, sets:to_list( SubProperties )},
	{ok, PropertyInfo}.
	
explicitSuperProperties( _, {Receiver, QID}, #property{ superProperties = SuperProperties } = PropertyInfo ) ->
	Receiver ! {QID, sets:to_list( SuperProperties )},
	{ok, PropertyInfo}.
	
isSubProperty( Property, {Receiver, QID} = ID, #property{ subProperties = SubProperties } = PropertyInfo ) ->
	Contains = sets:is_element( Property, SubProperties ),
	case Contains of
		true -> Receiver ! {QID, {true, 0}};
		false -> 
			Receiver ! {QID, {false, sets:size( SubProperties )}},
			sendToAll( SubProperties, isSubProperty, Property, ID )
	end,
	{ok, PropertyInfo}.
	
isExplicitSubProperty( Property, {Receiver, QID}, #property{ subProperties = SubProperties } = PropertyInfo ) ->
	Contains = sets:is_element( Property, SubProperties ),
	Receiver ! {QID, Contains},
	{ok, PropertyInfo}.

	
domain( Class, {Receiver, QID}, #property{ url = Self, domains = Domains } = PropertyInfo ) ->
	Class ! {domainOf, Self, {self(), QID}},
	NewDomains = sets:add_element( Class, Domains ),
	receive
		{QID, ok} -> Receiver ! {QID, ok}
	end,
	{ok, PropertyInfo#property{ domains = NewDomains }}.
	
	
getDomains( RespondingClass, {Receiver, QID} = ID, #property{ domains = Domains, superProperties = SuperProperties } = PropertyInfo ) ->
	Response = case RespondingClass of
		[] -> sets:to_list( Domains );
		_ -> lists:map( fun(Domain) -> {RespondingClass, atom_to_list( Domain )} end, sets:to_list( Domains ) )
	end,
	Receiver ! {QID, {Response, sets:size( SuperProperties )} },
	sendToAll( SuperProperties, getDomains, RespondingClass, ID ),
	{ok, PropertyInfo}.
	
isDomain( Class, {Receiver, QID} = ID, #property{ domains = Domains, superProperties = SuperProperties } = PropertyInfo ) ->
	Contains = sets:is_element( Class, Domains ),
	case Contains of
		true -> Receiver ! {QID, {true, 0}};
		false ->
			Receiver ! {QID, {false, sets:size( SuperProperties )}},
			sendToAll( SuperProperties, isDomain, Class, ID )
	end,
	{ok, PropertyInfo}.
	
	
range( Class, {Receiver, QID}, #property{ url = Self, ranges = Ranges } = PropertyInfo ) ->
	Class ! {rangeOf, Self, {self(), QID}},
	NewRanges = sets:add_element( Class, Ranges ),
	receive
		{QID, ok} -> Receiver ! {QID, ok}
	end,
	{ok, PropertyInfo#property{ ranges = NewRanges }}.
	
getRanges( RespondingClass, {Receiver, QID} = ID, #property{ ranges = Ranges, superProperties = SuperProperties } = PropertyInfo ) ->
	Response = case RespondingClass of
		[] -> sets:to_list( Ranges );
		_ -> lists:map( fun(Range) -> {RespondingClass, atom_to_list( Range )} end, sets:to_list( Ranges ) )
	end,
	Receiver ! {QID, {Response, sets:size( SuperProperties )}},
	sendToAll( SuperProperties, getRanges, RespondingClass, ID ),
	{ok, PropertyInfo}.
	
isRange( Class, {Receiver, QID} = ID, #property{ ranges = Ranges, superProperties = SuperProperties } = PropertyInfo ) ->
	Contains = sets:is_element( Class, Ranges ),
	case Contains of
		true -> Receiver ! {QID, {true, 0}};
		false ->
			Receiver ! {QID, {false, sets:size( SuperProperties )}},
			sendToAll( SuperProperties, isRange, Class, ID )
	end,
	{ok, PropertyInfo}.
	
%% handling of members related messages

add( {S, O} = Pair, {Receiver, QID}, #property{ pairs = Pairs, reversed = Reversed } = PropertyInfo ) ->	
	ets:insert( Pairs, Pair ),
	ets:insert( Reversed, {O, S} ),
	Receiver ! {QID, ok},
	%spreadActivation( addedNotificationTo( PropertyInfo ), Url, {nil, QID} ),
	{ok, PropertyInfo}.

%size( _, {Receiver, QID}, #property{ totalSize = Size } = PropertyInfo ) ->
%	Receiver ! {QID, Size},
%	{ok, PropertyInfo}.

explicitSize( _, {Receiver, QID}, #property{ pairs = Pairs } = PropertyInfo ) ->
	Size = ets:info( Pairs, size ),
	Receiver ! {QID, Size},
	{ok, PropertyInfo}.
	
	
contains( {S, O} = Pair, {Receiver, QID} = ID, #property{ pairs = Pairs, subProperties = SubProperties } = PropertyInfo ) ->
	PairsForSubject = ets:lookup( Pairs, S ),
	Contains = lists:keymember( O, 2, PairsForSubject ),
	case Contains of
		true -> Receiver ! {QID, {true, 0}};
		false ->
			Receiver ! {QID, {false, sets:size( SubProperties )}},
			sendToAll( SubProperties, contains, Pair, ID )
	end,
	{ok, PropertyInfo}.
	
explicitlyContains( {S, O}, {Receiver, QID}, #property{ pairs = Pairs } = PropertyInfo ) ->
	PairsForSubject = ets:lookup( Pairs, S ),
	Contains = lists:keymember( O, 2, PairsForSubject ),
	Receiver ! {QID, Contains},
	{ok, PropertyInfo}.
	
	
pairsParticipants( #property{ subProperties = SubProperties } ) ->
	[{SubProperties, pairs}].
	
pairs( _, {Receiver, QID} = ID, #property{ pairs = Pairs } = PropertyInfo ) ->
	Participants = pairsParticipants( PropertyInfo ),
	Receiver ! {QID, {ets:tab2list( Pairs ), toBeActivatedCount( Participants )}},
	spreadActivation( Participants, [], ID ),
	{ok, PropertyInfo}.
	
explicitPairs( _, {Receiver, QID}, #property{ pairs = Pairs } = PropertyInfo ) ->
	Res = ets:tab2list( Pairs ),
	Receiver ! {QID, Res},
	{ok, PropertyInfo}.
	
	
subjectsParticipants( #property{ subProperties = SubProperties } ) ->
	[{SubProperties, subjects}].
			
subjects( RespondingClass, {Receiver, QID} = ID, #property{ pairs = Pairs } = PropertyInfo ) ->
	Response = case RespondingClass of
		[] -> lists:map( fun([S]) -> S end, ets:match( Pairs, {'$1', '_'} ) );
		_ -> lists:map( fun([S]) -> {RespondingClass, S} end, ets:match( Pairs, {'$1', '_'} ) )
	end,
	Participants = subjectsParticipants( PropertyInfo ),
	Receiver ! {QID, {Response, toBeActivatedCount( Participants ) }},
	spreadActivation( Participants, RespondingClass, ID ),
	{ok, PropertyInfo}.
	
%explicitSubjects( _, {Receiver, QID}, #property{ pairs = Pairs } = PropertyInfo ) ->
%	Subjects = lists:map( fun([S]) -> S end, ets:match( Pairs, {'$1', '_'} ) ),
%	Receiver ! {QID, Subjects},
%	{ok, PropertyInfo}.
	

objectsParticipants( #property{ subProperties = SubProperties } ) ->
	[{SubProperties, objects}].	
	
objects( RespondingClass, {Receiver, QID} = ID, #property{ reversed = Reversed } = PropertyInfo ) ->
	Response = case RespondingClass of
		[] -> lists:map( fun([O]) -> O end, ets:match( Reversed, {'$1', '_'} ) );
		_ -> lists:map( fun([O]) -> {RespondingClass, O} end, ets:match( Reversed, {'$1', '_'} ) )
	end,
	Participants = objectsParticipants( PropertyInfo ),
	Receiver ! {QID, {Response, toBeActivatedCount( Participants )}},
	spreadActivation( Participants, RespondingClass, ID ),
	{ok, PropertyInfo}.
	

subjectsForParticipants( #property{ subProperties = SubProperties } ) ->
	[{SubProperties, subjectsFor}].

subjectsFor( Object, {Receiver, QID} = ID, #property{ reversed = Reversed } = PropertyInfo ) ->
	Set = [S || {_O, S} <- ets:lookup( Reversed, Object )],
	Participants = subjectsForParticipants( PropertyInfo ),
	Receiver ! {QID, {Set, toBeActivatedCount( Participants )}},
	spreadActivation( Participants, Object, ID ),
	{ok, PropertyInfo}.



subjectsForAsPairsParticipants( #property{ subProperties = SubProperties } ) ->
	[{SubProperties, subjectsForAsParticipants}].

subjectsForAsPairs( {Object, RespondingProperty} = Args, {Receiver, QID} = ID, #property{ reversed = Reversed } = PropertyInfo ) ->
	Response = lists:map( fun(Subject) -> {RespondingProperty, Subject} end, [S || {_O, S} <- ets:lookup( Reversed, Object )] ),
	Participants = subjectsForParticipants( PropertyInfo ),
	Receiver ! {QID, {Response, toBeActivatedCount( Participants )}},
	spreadActivation( Participants, Args, ID ),
	{ok, PropertyInfo}.




explicitSubjectsFor( Object, {Receiver, QID}, #property{ reversed = Reversed } = PropertyInfo ) ->
	Set = [S || {_O, S} <- ets:lookup(Reversed, Object)],
	Receiver ! {QID, Set},
	{ok, PropertyInfo}.
	
	
	
objectsForParticipants( #property{ subProperties = SubProperties } ) ->
	[{SubProperties, objectsFor}].

objectsFor( Subject, {Receiver, QID} = ID, #property{ pairs = Pairs } = PropertyInfo) ->
	Set = [O || {_S, O} <- ets:lookup( Pairs, Subject )],
	Participants = objectsForParticipants( PropertyInfo ),
	Receiver ! {QID, {Set, toBeActivatedCount( Participants )}},
	spreadActivation( Participants, Subject, ID ),
	{ok, PropertyInfo}.
	
objectsForAsPairsParticipants( #property{ subProperties = SubProperties } ) ->
	[{SubProperties, objectsForAsPairs}].

objectsForAsPairs( {Subject, RespondingProperty} = Args, {Receiver, QID} = ID, #property{ pairs = Pairs } = PropertyInfo) ->
	Response = lists:map( fun(O) -> {RespondingProperty, O} end, [O || {_S, O} <- ets:lookup( Pairs, Subject )] ),
	Participants = objectsForParticipants( PropertyInfo ),
	Receiver ! {QID, {Response, toBeActivatedCount( Participants )}},
	spreadActivation( Participants, Args, ID ),
	{ok, PropertyInfo}.
	
explicitObjectsFor( Subject, {Receiver, QID}, #property{ pairs = Pairs } = PropertyInfo ) ->
	Set = [O || {_S, O} <- ets:lookup( Pairs, Subject )],
	Receiver ! {QID, Set},
	{ok, PropertyInfo}.
	
	
% statistics
% 172
computeStatistics( _, {Receiver, QID}, 
		#property{ url = Url, pairs = Pairs, subProperties = SubProperties } = PropertyInfo ) ->
	
	sendToAll( SubProperties, computeStatistics, [], {self(), QID} ),% io:format("~p ~p", [OC, SC]),
	ObjCounts = ets:foldl( fun({_, O}, Dict) -> dict:update_counter( O, 1, Dict ) end, dict:new(), Pairs ),

	AvgObjOccurence = try ets:info( Pairs, size ) / dict:size( ObjCounts ) catch error:badarith -> 0 end,
		
	ExplicitStats = #property_stats{ 
		pairsCount = ets:info( Pairs, size ),
		avgObjectOccurence = AvgObjOccurence },
%	io:format("~n[~p] explicit: ~p", [Url, ExplicitStats]),
	Fun = 
		fun( {#property_stats{ pairsCount = PC, avgObjectOccurence = Avg }, Dict}, 
			 	{#property_stats{ pairsCount = TotalPC, avgObjectOccurence = TotalAvg}, TotalDict } ) ->
%					io:format("[~p] received: ~p ~p~n", [Url, PC, Avg]),
			{#property_stats{ pairsCount = PC + TotalPC, avgObjectOccurence = (Avg + TotalAvg)/2 }, 
			 dict:merge( fun statsDictMerge/3, Dict, TotalDict ) } end, % merging should occur only for interpreted properties
%	io:format("~n"),
	{Stats, Dict} = foldResponses( QID, Fun, fun identity/1,
		{ExplicitStats, dict:from_list( statsForInterpretedProperties( PropertyInfo ) )}, sets:size( SubProperties )),

	NewDict = dict:store( Url, Stats, Dict ),

	% receiver collects only direct children, so children count is 0
	Receiver ! {QID, {{Stats, NewDict}, 0}},	
	{ok, PropertyInfo}.
	
statsForInterpretedProperties( #property{ subProperties = SubProperties, domains = Domains, ranges = Ranges }) ->
	[{?rdfs?subPropertyOf, #property_stats{ pairsCount = sets:size( SubProperties ) + 1, avgObjectOccurence = 1 }},
	{?rdfs?domain, #property_stats{ pairsCount = sets:size( Domains ), avgObjectOccurence = sets:size( Domains ) }},
	{?rdfs?range, #property_stats{ pairsCount = sets:size( Ranges ), avgObjectOccurence = sets:size( Ranges ) }}].
	% rdf:type, rdfs:subClassOf are defined in class - probably ?
	
		
