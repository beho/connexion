-module (connexion).
-compile(export_all).
-export ([
	setup/0,
	initialize/2,
	stop/1,
	interpretedProperties/0
	]).

-include ("actor.hrl").

-include ("namespaces.hrl").

-include ("vocabularies/rdf.hrl").
-include ("vocabularies/rdfs.hrl").

-record (connexion_state, {
	statistics,
	namespaces = #namespaces{}
	%{0, dict:new(), dict:new() }% Count, uri -> number, number -> {prefix, uri}
}).



% Public

setup() ->
	new( ?MODULE ).
	
initialize( _, _ ) ->
	loadMetaModel(),
	#connexion_state{ statistics = model:statistics() }.
	
stop( _ ) ->
	Properties = model:membersOf( rdf:property() ),
	Classes = model:membersOf( rdfs:class() ),
	stopProperties( Properties ),
	stopClasses( Classes ).
	
methodFor( Selector ) ->
	case Selector of
		?M( refreshStatistics );
		?M( getStatistics );
		
		?M( registerNamespaces );
		?M( registeredNamespaces );
		
		?DNU_HANDLER
	end.
	
refreshStatistics( _, {Receiver, QID}, State) ->
	Receiver ! {QID, ok},
	{ok, State#connexion_state{ statistics = model:statistics() }}.
	
getStatistics( _, {Receiver, QID}, #connexion_state{ statistics = Statistics } = State ) ->
	Receiver ! {QID, Statistics},
	{ok, State}.
	
registerNamespaces( Namespaces, {_Receiver, _QID}, #connexion_state{ namespaces = Nss } = State ) ->
	NewNss = namespaces:merge( Namespaces, Nss ),
	% NewNss = lists:foldl( 
	% 	fun({Prefix, NsUri}, Acc) ->
	% 		case dict:is_key( Prefix, Nss#namespaces.prefixToUri ) of
	% 			false ->
	% 				#namespaces{
	% 					prefixToUri = dict:store( Prefix, NsUri, Acc#namespaces.prefixToUri),
	% 					uriToPrefix = dict:store( NsUri, Prefix, Acc#namespaces.uriToPrefix)
	% 				};
	% 			true -> Acc
	% 		end
	% 	end,
	% 	Nss, Namespaces ),
	{ok, State#connexion_state{ namespaces = NewNss }}.
	
registeredNamespaces( _, {Receiver, QID}, #connexion_state{ namespaces = Namespaces } = State ) ->
	Receiver ! {QID, Namespaces},
	{ok, State}.
	
	
interpretedProperties() ->
	rdf:interpretedProperties() ++ rdfs:interpretedProperties().
	

% Interface

refreshStatistics() ->
	QID = make_ref(),
	?MODULE ! {refreshStatistics, [], {self(), QID}},
	receive
		{QID, ok} -> ok
	end.

registerNamespaces( Namespaces ) ->
	io:format("registering namespace: ~p~n", [dict:to_list( Namespaces#namespaces.uriToPrefix )]),
	QID = make_ref(),
	?MODULE ! {registerNamespaces, Namespaces, {self(), QID}}.
	
registeredNamespaces() ->
	QID = make_ref(),
	?MODULE ! {registeredNamespaces, [], {self(), QID}},
	receive
		{QID, Namespaces} -> Namespaces
	end.

% Private


loadMetaModel() ->
	class:new(?'rdfs:Resource'),
	import:triplesFromFile( "22-rdf-syntax-ns" ),
	import:triplesFromFile( "rdf-schema" ).

	
stopProperties( Properties ) ->
	lists:map( fun( Property ) -> list_to_atom( Property ) ! stop end, Properties ).
	
stopClasses( Classes ) ->
	lists:map( fun stopClass/1, Classes ),
	?'rdfs:Class' ! stop.
	
stopClass( Class ) ->
	ClassAtom = list_to_atom( Class ),
	case ClassAtom of
		?'rdfs:Class' -> ok;
		_ -> ClassAtom ! stop
	end.
		
	