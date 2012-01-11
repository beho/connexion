%% @author Svatopluk Sperka (isperka@fit.vutbr.cz)
%% {@date}

-module (rdf_parser).

-export ([
	for/1
]).
-compile(export_all).

-include ("actor.hrl").

%% @doc Path to raptor_parser port relative to the project's root.
-define( RAPTOR_PATH, "./ebin/raptor_parser" ).

%% @doc Directory where files to be imported are expected.
-define( IMPORT_DIR, "./import/" ).

%% @doc Number of triples to be sent in one batch from port.
-define( TRIPLES_IN_BATCH, 250 ).

%% @doc Messsage ID to be sent to port when preparing file for parsing.
-define( PREPARE_FILE_FOR_PARSING, 1 ).

%% @doc Message ID for request of a next batch.
-define( NEXT_BATCH, 2 ).


%% @doc Constructor of a parser for a given filename.
%% @spec for( atom() ) -> ({ok, pid()} | {error | no_such_file})
for( Filename ) when is_atom( Filename ) ->
	for( atom_to_list( Filename ) );

%% @spec for( list() ) -> ({ok, pid()} | {error | no_such_file})
for( Filename ) when is_list( Filename ) ->
	Filepath = ?IMPORT_DIR ++ Filename,
	case filelib:is_regular( Filepath ) of
		true -> {ok, new( Filepath )};
		false -> {error, no_such_file}
	end.

%% @doc Opens port and sends it a message to prepare a file for parsing.
initialize( _, Filepath ) ->
	%register( ?MODULE, self() ),
	process_flag( trap_exit, true ),
	Port = open_port( {spawn, ?RAPTOR_PATH}, [{packet, 2}, binary] ),
	%io:format("port: ~p~n", [Port]),
	prepareFileForParsing( Port, Filepath, ?TRIPLES_IN_BATCH  ),
	Port.

%% @doc Closes port, exiting actor is handled by actor library.
%% @TODO write some closing code.
stop( Port ) ->
	erlang:port_close(Port).

%% @doc Method selecting.	
methodFor( Selector ) ->
	case Selector of
		?M( nextBatch );
		
		?DNU_HANDLER
	end.

%% @doc Sends necessary information to port to prepare itself for parsing of a file.	
prepareFileForParsing( Port, Filepath, TriplesInBatchLimit ) ->
	port_command( Port, term_to_binary( {?PREPARE_FILE_FOR_PARSING, list_to_atom(Filepath), TriplesInBatchLimit} ) ),
	receive
		{Port, {data, Data}} -> 
			%io:format("opening file: ~s~n", [binary_to_term( Data )] ),
			binary_to_term( Data )
	end.
	
%% @doc Requests next batch of triples.	
nextBatch( _Args, {From, QID}, Port ) ->
	port_command( Port, term_to_binary( {?NEXT_BATCH} ) ),
	receive
		{Port, {data, Data}} ->
			Tuple = binary_to_term( Data ),
			%io:format("~p~n", [Tuple]),
			From ! {QID, Tuple},
			{Tag, _, _} = Tuple
	end,
	case Tag of
		batch -> {ok, Port};
		finished -> stop
	end.
	

% =========== Legacy code intended for easier testing and debugging	

% start() ->
% 	spawn_link( ?MODULE, init, [] ).
% 
% init() ->
% 	register( ?MODULE, self() ),
% 	process_flag( trap_exit, true ),
% 	Port = open_port( {spawn, ?RAPTOR_PATH}, [{packet, 2}, binary] ),
% 	loop( Port ).
% 
% stop() ->
% 	?MODULE ! stop.
% 	
% loop( Port ) ->
% 	receive
% 		{prepareFileParsing, FilePath, TriplesInBatchLimit} ->
% 			port_command( Port, term_to_binary( {?PREPARE_FILE_FOR_PARSING, FilePath, TriplesInBatchLimit} ) ),
% 			receive
% 				{Port, {data, Data}} -> 
% 					io:format("opening file: ~s~n", [binary_to_term( Data )] ),
% 					loop( Port );
% 					
% 				Msg -> io:format("received ~w~n", [Msg])
% 			end;
% 			
% 		nextBatch ->
% 			port_command( Port, term_to_binary( {?NEXT_BATCH} ) ),
% 			receive
% 				{Port, {data, Data}} ->
% 					{Tag, Namespaces, Triples} = binary_to_term( Data ),
% 				%	io:format( "getting triples: ~w~n", [Tag] ),
% 				%	io:format( "ns: ~p\n", [Namespaces]),
% 					%io:format( "~w", [Data]),
% 				%	lists:map( fun({S, P, O}) -> io:format( "~s ~s ~s~n", [S, P, O]) end, List ),
% 					loop( Port )
% 			end;
% 		
% 		% EXIT MESSAGES
% 			
% 		stop ->
% 			%port_command( Port, term_to_binary( {4} ) ),
% 			%receive
% 			%	{Port, {data, _Data}} -> % no need to check Data - port only returns ok
% 					erlang:port_close(Port),
% 					exit(normal);
% 			%end;
% 		
% 		_ -> loop( Port )
% 	end.
