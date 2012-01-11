-module (import).

-export ([triplesFromFile/1, triples/1, triples/2, triple/3]).

-include ("namespaces.hrl").

-include ("vocabularies/rdf.hrl").
-include ("vocabularies/rdfs.hrl").

triplesFromFile( Filename ) ->
	case rdf_parser:for( Filename ) of
		{ok, PID} ->
			Imported = handleTripleBatchesFrom( PID ),
			io:format("Imported ~p triples from ~p~n", [Imported, Filename]);
		{error, no_such_file} -> no_such_file
	end.
	
handleTripleBatchesFrom( PID ) ->
	handleTripleBatchesFrom( PID, #namespaces{}, 0 ).
	
handleTripleBatchesFrom( PID, Nss, Imported ) ->
	QID = make_ref(),
	PID ! {nextBatch, [], {self(), QID}},
	receive
		{QID, {batch, Namespaces, Triples}} ->
			%io:format("defined namespaces ~p~n", [Namespaces]),
			NewNss = namespaces:add( Namespaces, Nss ),
%			connexion:registerNamespaces( Namespaces ),
			CurrentBatch = import:triples( Triples, NewNss ),
			handleTripleBatchesFrom( PID, NewNss, Imported + CurrentBatch );
		{QID, {finished, Namespaces, Triples}} ->
%			io:format("defined namespaces ~p~n", [Namespaces]),
			NewNss = namespaces:add( Namespaces, Nss ),
			Total = Imported + import:triples( Triples, Nss ),
			connexion:registerNamespaces( NewNss ),
			Total
	end.

translate( Triple, _ ) ->
	Triple.


%% @TODO use foldr to collect possible errors ?
triples( List ) ->
	triples( List, #namespaces{} ).

triples( List, Namespaces ) ->
	lists:foldl( fun(Triple, C) ->
		{S, P, O} = translate( Triple, Namespaces ),
		triple( S, P, O ), C + 1 end, 0, List ).

%% @TODO should filter where interpreted subjects are S or P or how to treat it ?
 
triple( Resource, ?rdf?type, Class ) ->
	model:addMember( Class, Resource );
	
triple( Class, ?rdfs?subClassOf, SuperClass ) ->
	model:subClassOf( Class, SuperClass );
	
triple( Property, ?rdfs?subPropertyOf, SuperProperty ) ->
	model:subPropertyOf( Property, SuperProperty );
	
triple( Property, ?rdfs?domain, Class ) ->
	model:domain( Property, Class );
	
triple( Property, ?rdfs?range, Class ) ->
	model:range( Property, Class );
	
triple( Subject, Property, Object ) ->
	model:addPair( Property, {Subject, Object} ).
