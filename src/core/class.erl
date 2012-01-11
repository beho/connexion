-module (class).

-include ("actor.hrl").

-include ("stats.hrl").
-include ("vocabularies/rdfs.hrl").

-record (class,
	{url,
	 members,
	 participantsSize,
	 subClasses = sets:new(), superClasses = sets:new(),
	 domainOf = sets:new(), rangeOf = sets:new()
	}).

%% Actor Methods

initialize( Url, _ ) ->
	% all classes are members of rdfs:Class, but we must not try to add it to
	% itself because it would lead to infinite loop.
	UrlAsList = atom_to_list( Url ),
	{Members, SubClasses, SuperClasses} =
		case Url of
			?'rdfs:Class' ->
				{gb_sets:from_list( [?rdfs?Class] ), sets:new(), sets:from_list([ ?'rdfs:Resource' ])};
			?'rdfs:Resource' ->
				model:addMember( ?rdfs?Class, UrlAsList ),
				{gb_sets:new(), sets:from_list( [?'rdfs:Class']), sets:new()};
			_ ->
				QID = make_ref(),
				?'rdfs:Resource' ! {superClassOf, Url, {self(), QID}},
				receive
					{QID, ok} -> ok
				end,
				model:addMember( ?rdfs?Class, UrlAsList ),
				{gb_sets:new(), sets:new(), sets:from_list( [?'rdfs:Resource'] )}
		end,
	
	% 	case Url =:= ?'rdfs:Class' of
	% 		true ->
	% 			gb_sets:add_element( ?rdfs?Class, gb_sets:new() );
	% 		false -> 
	% 			model:addMember( ?rdfs?Class, UrlAsList ),
	% 			gb_sets:new()
	% 	end,
	% 
	% SuperClasses =
	% 	case Url =:= ?'rdfs:Resource' of
	% 		true ->
	% 			sets:new();
	% 		false ->
	% 			QID = make_ref(),
	% 			?'rdfs:Resource' ! {superClassOf, Url, {QID, self()}},
	% 				io:forma("zz"),
	% 			receive
	% 				{QID, ok} -> io:format("xx"),ok
	% 			end,
	% 			sets:from_list( [?'rdfs:Resource'] )
	% 	end,

	#class{ url = UrlAsList, members = Members, subClasses = SubClasses, superClasses = SuperClasses, participantsSize = gb_sets:size( Members ) }.

stop( _ ) ->
	ok.
	
methodFor( Selector ) ->
	case Selector of
		?M( subClassOf );
		?M( subClasses );
		?M( explicitSubClasses );
		?M( isSubClass );
		?M( isExplicitSubClass );
		
		?M( superClassOf );
		?M( superClasses );
		?M( explicitSuperClasses );
		
		?M( domainOf );
		?M( getDomainOf );
		?M( rangeOf );
		?M( getRangeOf );
		
		?M( add );
		
		?M( contains );
		?M( explicitlyContains );
		
		?M( members );
		?M( explicitMembers );
		
%		?M( size );
%		?M( explicitSize );
		
		?M( computeStatistics );
		
		
		?DNU_HANDLER
	end.

%% Handling of structure related messages

subClassOf( Class, {Receiver, QID}, #class{ url = Self, superClasses = SuperClasses } = ClassInfo) ->
	NewSuperClasses = set_actor_helper:subSetOf( Self, Class, SuperClasses, superClassOf, QID ),
	Receiver ! {QID, ok},
	{ok, ClassInfo#class{ superClasses = NewSuperClasses } }.
		
superClassOf( Class, {Receiver, QID}, #class{ subClasses = SubClasses } = ClassInfo ) ->
	NewSubClasses = set_actor_helper:superSetOf( Class, SubClasses ),
	Receiver ! {QID, ok},
	{ok, ClassInfo#class{ subClasses = NewSubClasses } }.

	
subClasses( RespondingClass, {Receiver, QID} = ID, #class{ url = Self, subClasses = SubClasses } = ClassInfo ) ->
	Response = case RespondingClass of
		[] -> [Self];
		_ -> [{RespondingClass, Self}]
	end,
	Receiver ! {QID, {Response, sets:size( SubClasses )}},
	sendToAll( SubClasses, subClasses, RespondingClass, ID ),
	{ok, ClassInfo}.
	
superClasses( _, {Receiver, QID} = ID, #class{ url = Self, superClasses = SuperClasses } = ClassInfo ) ->
	Receiver ! {QID, {[Self], sets:size( SuperClasses )}},
	sendToAll( SuperClasses, superClasses, [], ID ),
	{ok, ClassInfo}.

	
explicitSubClasses( _, {Receiver, QID}, #class{ subClasses = SubClasses} = ClassInfo ) ->
	Receiver ! {QID, sets:to_list( SubClasses )},
	{ok, ClassInfo}.
	
explicitSuperClasses( _, {Receiver, QID}, #class{ superClasses = SuperClasses} = ClassInfo ) ->
	Receiver ! {QID, sets:to_list( SuperClasses )},
	{ok, ClassInfo}.
	
	
isSubClass( Class, {Receiver, QID} = ID, #class{ subClasses = SubClasses } = ClassInfo) ->
	Contains = sets:is_element( Class, SubClasses ),
	case Contains of
		true -> Receiver ! {QID, {true, 0}};
		false -> 
			Receiver ! {QID, {false, sets:size( SubClasses )}},
			sendToAll( SubClasses, isSubClass, Class, ID )
	end,
	{ok, ClassInfo}.
	
isExplicitSubClass( Class, {Receiver, QID}, #class{ subClasses = SubClasses } = ClassInfo ) ->
	Contains = sets:is_element( Class, SubClasses ),
	Receiver ! {QID, Contains},
	{ok, ClassInfo}.

	
domainOf( Property, {Receiver, QID}, #class{ domainOf = DomainOf} = ClassInfo ) ->
	NewDomainOf = sets:add_element( Property, DomainOf ),
	Receiver ! {QID, ok},
	{ok, ClassInfo#class{ domainOf = NewDomainOf }}.
	
getDomainOf( _, {Receiver, QID} = ID, #class{ domainOf = DomainOf, superClasses = SuperClasses } = ClassInfo ) ->
	Receiver ! {QID, {sets:to_list( DomainOf ), sets:size( SuperClasses )}},
	sendToAll( SuperClasses, getDomainOf, [], ID ),
	{ok, ClassInfo}.
	
	
rangeOf( Property, {Receiver, QID}, #class{ rangeOf = RangeOf } = ClassInfo ) ->
	NewRangeOf = sets:add_element( Property, RangeOf ),
	Receiver ! {QID, ok},
	{ok, ClassInfo#class{ rangeOf = NewRangeOf }}.
	
getRangeOf( _, {Receiver, QID} = ID, #class{ rangeOf = RangeOf, superClasses = SuperClasses } = ClassInfo ) ->
	Receiver ! {QID, {sets:to_list( RangeOf ), sets:size( SuperClasses )}},
	sendToAll( SuperClasses, getRangeOf, [], ID ),
	{ok, ClassInfo}.


%% handling of members related messages
	
add( URL, {Receiver, QID}, #class{ members = Members } = ClassInfo ) ->
	NewMembers = gb_sets:add_element( URL, Members ),
	Receiver ! {QID, ok},
	%spreadActivation( addedNotificationTo( ClassInfo ), Url, {nil, QID}),
	{ok, ClassInfo#class{ members = NewMembers }}.

%size( _, {Receiver, QID}, #class{ members = Members, participantsSize = Size } = ClassInfo ) ->
%	Receiver ! {QID, gb_sets:size( Members ) + Size},
%	{ok, ClassInfo}.

%explicitSize( _, {Receiver, QID}, #class{ members = Members } = ClassInfo ) ->
%	Size = gb_sets:size( Members ),
%	Receiver ! {QID, Size},
%	{ok, ClassInfo}.


contains( URL, {Receiver, QID} = ID, #class{ members = Members, subClasses = SubClasses } = ClassInfo) ->
	Contains = gb_sets:is_element( URL, Members ),
	case Contains of
		true -> Receiver ! {QID, {true, 0}};
		false ->
			Receiver ! {QID, {false, sets:size( SubClasses )}},
			sendToAll( SubClasses, contains, URL, ID )
	end,
	{ok, ClassInfo}.
	
explicitlyContains( URL, {Receiver, QID}, #class{ members = Members } = ClassInfo) ->
	Contains = gb_sets:is_member( URL, Members ),
	Receiver ! {QID, Contains},
	{ok, ClassInfo}.
	

membersParticipants( #class{ subClasses = SubClasses, domainOf = DomainOf, rangeOf = RangeOf } ) ->
	[{SubClasses, members},
	 {DomainOf, subjects},
	 {RangeOf, objects}].
	
members( RespondingClass, {Receiver, QID} = ID, #class{ members = Members } = ClassInfo) ->
	Participants = membersParticipants( ClassInfo ),
	Response = case RespondingClass of
		[] -> gb_sets:to_list( Members );
		_ -> lists:map( fun(Member) -> {RespondingClass, Member} end, gb_sets:to_list(Members) )
	end,
	Receiver ! {QID, {Response, toBeActivatedCount( Participants )}},
	spreadActivation( Participants, RespondingClass, ID ),

	{ok, ClassInfo}.
	

explicitMembers( _, {Receiver, QID}, #class{ members = Members } = ClassInfo ) ->
	List = gb_sets:to_list(Members),
	Receiver ! {QID, List},
	{ok, ClassInfo}.
	


computeStatistics( _, {Receiver, QID}, #class{ url = Url, members = Members, subClasses = SubClasses } = ClassInfo ) ->
	sendToAll( SubClasses, computeStatistics, [], {self(), QID} ),
	
	ExplicitStats = #class_stats{ membersCount = gb_sets:size( Members ) },
%	MembersCount = gb_sets:size( Members ),
	PropertyStats = [{?rdfs?subClassOf, #property_stats{ pairsCount = sets:size( SubClasses ), avgObjectOccurence = 1 }}],
%		{?rdf?type, #property_stats{ pairsCount = MembersCount, avgObjectOccurence = MembersCount }}],
	
	Fun =
		fun( {#class_stats{ membersCount = MC }, PropDict, ClassDict},
			 {#class_stats{ membersCount = TotalMC }, TotalPropDict, TotalClassDict} ) ->
				
			{#class_stats{ membersCount = MC + TotalMC },
			 dict:merge( fun statsDictMerge/3, PropDict, TotalPropDict),
			 dict:merge( fun statsDictMerge/3, ClassDict, TotalClassDict)}
		end,
	
	{ClassStats, PropertyStatsDict, ClassStatsDict} = 
		foldResponses( QID, Fun, fun identity/1, 
			{ExplicitStats, dict:from_list( PropertyStats ), dict:new()}, sets:size( SubClasses ) ),
	
	NewClassStatsDict = dict:store( to_list(Url), ClassStats, ClassStatsDict ),
	
	Receiver ! {QID, {{ClassStats, PropertyStatsDict, NewClassStatsDict}, 0}},
	{ok, ClassInfo}.
	
	

