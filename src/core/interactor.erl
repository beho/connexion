-module (interactor).

-include ("actor.hrl").

-compile (export_all).

-record (interaction, { socket, output_to, queries }).

initialize( _, _ ) ->
	#interaction{ queries = dict:new() }.
	
stop( _ ) ->
	ok.
	
methodFor( Selector ) ->
	case Selector of
		membersOf -> fun membersOf/2
		%sendOutputTo -> fun sendOutputTo/2
	end.


%% ========== Adding ==========

%% === Class ===

addMember( Class, URL, _Interaction ) ->
	model_creation:addMember( Class, URL ).
	
subClassOf( SubClass, Class, _Interaction ) ->
	model_creation:subClassOf( SubClass, Class ).
	
%% === Property ===	
	
addPair( Property, Pair, _Interaction ) ->
	model_creation:addPair( Property, Pair ).
	
domain( Property, Class, _Interaction ) ->
	model_creation:domain( Property, Class ).
	
range( Property, Class, _Interaction ) ->
	model_creation:range( Property, Class ).
	

%% ========== Querying ==========

%% === Class ===

membersOf( Class, _Interaction ) ->
	model_querying:membersOf( Class ).

subClassesOf( Class, _Interaction ) ->
	model_querying:subClassesOf( Class ).
	
containsMember( Class, URL, _Interaction ) ->
	model_querying:containsMember( Class, URL ).

%% === Property ===

subPropertiesOf( Property, _Interaction ) ->
	model_querying:subPropertiesOf( Property ).

domainsOf( Property, _Interaction ) ->
	model_querying:domainsOf( Property ).
	
rangesOf( Property, _Interaction ) ->
	model_querying:rangesOf( Property ).
	
pairsFor( Property, _Interaction) ->
	model_querying:pairsFor( Property ).

containsPair( Property, Pair, _Interaction ) ->
	model_querying:containsPair( Property, Pair ).
	
subjectsFor( Property, Object, _Interaction ) ->
	model_querying:subjectsFor( Property, Object ).
	
objectsFor( Property, Subject, _Interaction ) ->
	model_querying:objectsFor( Property, Subject ).


	