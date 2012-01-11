// TODO add namespace handling (http://librdf.org/raptor/api/tutorial-parser-set-namespace-handler.html)
// TODO add bnodeID generating based on connexion's contents

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

//#include <ei.h>
//#include <erl_interface.h>
#include <raptor.h>

typedef unsigned char byte;

// message identifiers
#define PREPARE_FILE_FOR_PARSING   1
#define NEXT_BATCH                 2

// response tags
#define BATCH      "batch"
#define FINISHED   "finished"

// state of the parsing, substantially it holds triples to be sent.
typedef struct {
 	byte* buffer;
 	int recordsInBatchLimit;
 //	ETERM** batch;
	byte** batch;
 	int triplesInCurrentBatch;
 	int batchesSent;
 //	ETERM** namespaces;
	byte** namespaces;
 	int namespacesInCurrentBatch;
} parser_partial_state;


// ********* funtions for manipulating with in/out buffer
// int read_exact(byte *buf, int len)
// {
//   int i, got = 0;
//   do {
//     if( (i = read( 0, buf+got, len-got )) <= 0 ) 
//       return i;
//     got += i;
//   } while( got < len );
// 
//   return len;
// }
// 
// int read_cmd( byte *buf )
// {
// 	int len;
// 		
// 	if( read_exact(buf, 2) != 2 )
// 		return -1;
// 	
// 	len = (buf[0] << 8) | buf[1];
// 
// 	return read_exact( buf, len );
// }
// 
// int write_exact( byte *buf, int len )
// {
//   int i, wrote = 0;
//   do {
//     if ( (i = write( 1, buf+wrote, len-wrote )) <= 0 )
//       return i;
//     wrote += i;
//   } while( wrote < len );
// 
//   return len;
// }
// 
// // TODO should use resizable buffer and 4 byte length header ?
// int write_cmd( byte *buf, int len )
// {
//   byte li;
//   li = (len >> 8) & 0xff;
// 
//   write_exact( &li, 1 );
//   
//   li = len & 0xff;
// 
//   write_exact( &li, 1 );
//   return write_exact( buf, len );
// }
// ********* funtions for manipulating with in/out buffer


// writes (sends in erlang terms) triples held in current batch
// void writeBatch( parser_partial_state* state, char* tag ) {
// 	ETERM* triples = erl_mk_list( state->batch, state->triplesInCurrentBatch );
// 	ETERM* namespaces = erl_mk_list( state->namespaces, state->namespacesInCurrentBatch );
// 	ETERM* response = erl_format( "{~a, ~w, ~w}", tag, namespaces, triples );
// 
// 	erl_encode( response, state->buffer );
// 	write_cmd( state->buffer, erl_term_len( response ) );
// 	
// 	// according to documentation, whole response should be freed recursively. I truly hope so. Should test it somehow.
// 	erl_free_compound( response );
// }

// void sendAndWaitForRequest( parser_partial_state* state ) {
// 	writeBatch( state, BATCH );
// 
// 	state->triplesInCurrentBatch = 0;
// 	
// 	while( read_cmd( state->buffer ) > 0 ) {
// 		ETERM* request = erl_decode( state->buffer );
// 		//erl_err_msg("waiting");
// 		bool go = (ERL_INT_VALUE( erl_element( 1, request ) ) == NEXT_BATCH);
// 		erl_free_compound( request );
// 		
// 		if( go ) {
// 			break;
// 		}
// 	}
// }

void handleNamespace( void* user_data, raptor_namespace *nspace )
{
	byte *ns_uri;
	parser_partial_state* state = (parser_partial_state*) user_data;
	
	const byte* prefix = raptor_namespace_get_prefix( nspace );
	ns_uri = raptor_uri_as_string( raptor_namespace_get_uri( nspace ) );
	
//	erl_err_msg( "namespace defined: %s %s\n", prefix, ns_uri );
	printf( "namespace defined: %s %s\n", prefix, ns_uri );
	
//	ETERM* ns = erl_format( "{~w, ~w}", prefix, ns_uri );
	//printf("aa\n");
	state->namespaces[state->namespacesInCurrentBatch++] = ns_uri;
	//printf("xxx");
	// if batch is complete, send it and wait for request to continue
//	if( (state->triplesInCurrentBatch + state->namespacesInCurrentBatch) == state->recordsInBatchLimit ) {
//		sendAndWaitForRequest( state );
//	}
	// add namespace to it
}


// handler for triples. accumulates them in parser's state and ensures sending of batches to erlang process.
void handleTriple( void* user_data, const raptor_statement* triple )
{
	parser_partial_state* state = (parser_partial_state*) user_data;
	
	byte *subject_string, *predicate_string, *object_string;
	byte buffer[256];
//	ETERM *subject, *predicate, *object, *e_triple;
	
	switch( triple->subject_type ) {
		case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
			subject_string = raptor_uri_as_string( (raptor_uri*)triple->subject );
			break;
		
		case RAPTOR_IDENTIFIER_TYPE_LITERAL:
			subject_string = (byte*)triple->subject;
			break;
		
		case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
			sprintf( (char*)buffer, "_:%s", raptor_uri_as_string( (raptor_uri*)triple->subject ) );
	//		erl_err_msg("subject %s\n", buffer);
			subject_string = buffer;
			break;
			
		default:
//			erl_err_msg( "PARSER ERROR - unknown object type: %d %s\n", triple->subject_type, (byte*)triple->subject );
			subject_string = (byte*)"!!!";
			break;
		
	}
	
	//subject_string = raptor_uri_as_string( (raptor_uri*)triple->subject );
//	subject = erl_mk_estring( (char*)subject_string, strlen( (char*)subject_string ) );
	
	predicate_string = raptor_uri_as_string( (raptor_uri*)triple->predicate );
//	predicate = erl_mk_estring( (char*)predicate_string, strlen( (char*)predicate_string ) );
	
	switch( triple->object_type ) {
		case RAPTOR_IDENTIFIER_TYPE_RESOURCE:
			object_string = raptor_uri_as_string( (raptor_uri*)triple->object );
			break;
		
		case RAPTOR_IDENTIFIER_TYPE_LITERAL:
			object_string = (byte*)triple->object;
			break;
		
		case RAPTOR_IDENTIFIER_TYPE_ANONYMOUS:
			sprintf( (char*)buffer, "_:%s", raptor_uri_as_string( (raptor_uri*)triple->object ) );
	//		erl_err_msg("object %s\n", buffer);
			object_string = buffer;
			break;
			
		default:
	//		erl_err_msg( "PARSER ERROR - unknown object type: %d %s\n", triple->object_type, (byte*)triple->object );
			object_string = (byte*)"!!!";
			break;
	}
//	object = erl_mk_estring( (char*)object_string, strlen( (char*)object_string ) );
	
//	e_triple = erl_format( "{~w, ~w, ~w}", subject, predicate, object );
	state->batch[state->triplesInCurrentBatch++] = subject_string;


//	if( (state->triplesInCurrentBatch + state->namespacesInCurrentBatch) == state->recordsInBatchLimit ) {
//		sendAndWaitForRequest( state );
//	}
	printf("%s %s %s\n", subject_string, predicate_string, object_string);
}


int main( int argc, char** argv ) {
	byte buffer[65536];
	
//	ETERM *cmd, *response, *e_fileName, *e_tripleCount;
	
	raptor_parser* parser = NULL;
	byte* uri_string;
	raptor_uri uri, base_uri;

	char* fileName = argv[1];
	printf("%s\n", fileName);
						
	parser_partial_state state;
	
//	erl_init( NULL, 0 );
	raptor_init();
	
	//erl_err_msg("raptor version: %d.%d.%d.%d\n\n", raptor_version_major, raptor_version_minor, raptor_version_release, raptor_version_decimal );
	
//	while( read_cmd( buffer ) > 0 ) {
//		cmd = erl_decode( buffer );
		
//		int function = ERL_INT_VALUE( erl_element( 1, cmd ) );
		
		//erl_err_msg( "function %d\n", function );
//		switch( function ) {
//			case PREPARE_FILE_FOR_PARSING: // create a parser for a given file
//				e_fileName = erl_element( 2, cmd );
//				fileName = ERL_ATOM_PTR( e_fileName );
				
//				e_tripleCount = erl_element( 3, cmd );
			//	state.recordsInBatchLimit = 50//ERL_INT_VALUE( e_tripleCount );
				
				// file existence check is made in erlang code before creating a port
				// if we can't open file
				/*if( (f = fopen( fileName, "r" )) == NULL ) {
					response = erl_format( "{~a, ~a}", "error", "file" );
				} // if memory can't be allocated
				else*/ 
				if( (state.batch = malloc( state.recordsInBatchLimit * sizeof(byte*) )) == NULL ) {
					//response = erl_format( "{~a, ~a}", "error", "memory" );
					printf("memory\n");
				}
				else if( (state.namespaces = malloc( state.recordsInBatchLimit * sizeof(byte*) )) == NULL ) {
					printf("memory\n");
					//response = erl_format( "{~a, ~a}", "error", "memory" );
				}
				else {
					state.buffer = buffer;
							
					state.triplesInCurrentBatch = 0;
					state.batchesSent = 0;
					state.namespacesInCurrentBatch = 0;
					
					parser = raptor_new_parser( "rdfxml" );

					raptor_set_namespace_handler( parser, (void*) &state, handleNamespace );
					raptor_set_statement_handler( parser, (void*) &state, handleTriple );
					
					uri_string = raptor_uri_filename_to_uri_string( fileName );
					uri = raptor_new_uri( uri_string );
					
					base_uri = NULL;
					
//					response = erl_mk_atom( "ok" );
					
				// }
				
//				erl_encode( response, buffer );
//				write_cmd( buffer, erl_term_len( response ) );
				
//				erl_free_term( e_fileName );
//				erl_free_term( e_tripleCount );
//				erl_free_term( response );
//				break;
				
//			case NEXT_BATCH: 
				raptor_parse_file( parser, uri, base_uri );
				// TODO should pass filename as third param ?
				//raptor_parse_file_stream( parser, f, NULL, base_uri );
				//erl_err_msg( "back in main loop\n" );
				
//				writeBatch( &state, FINISHED );
				
				raptor_free_parser( parser );
				raptor_free_uri( uri );
				//raptor_free_uri( base_uri );
				raptor_free_memory( uri_string );
				
//				break;
			
			/*case 4: // close
				raptor_free_parser( parser );
				raptor_free_uri( uri );
				raptor_free_uri( base_uri );
				raptor_free_memory( uri_string );
				break;*/
			
//			default:
				// construct error term
//				erl_err_msg("default?");
//				return 1;
		}
		
		
//	}
	
	raptor_finish();
	//erl_err_msg("exit");
	return 0;
}
