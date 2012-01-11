-module (namespaces).

-export ([
	new/0,
	add/2,
	merge/2]).

-include ("namespaces.hrl").

new() ->
	#namespaces{}.
	
add( [], Namespaces ) ->
	Namespaces;
add( [{Prefix, Uri}|T], Namespaces ) ->
	NewNamespaces = case dict:is_key( Prefix, Namespaces#namespaces.prefixToUri ) of
		false ->
			#namespaces{
				prefixToUri = dict:store( Prefix, Uri, Namespaces#namespaces.prefixToUri),
				uriToPrefix = dict:store( Uri, Prefix, Namespaces#namespaces.uriToPrefix)
			};
		true -> Namespaces
	end,
	add( T, NewNamespaces );
	
add( New, Namespaces ) ->
	lists:foldl( 
		fun({Prefix, NsUri}, Acc) ->
			case dict:is_key( Prefix, Namespaces#namespaces.prefixToUri ) of
				false ->
					#namespaces{
						prefixToUri = dict:store( Prefix, NsUri, Acc#namespaces.prefixToUri),
						uriToPrefix = dict:store( NsUri, Prefix, Acc#namespaces.uriToPrefix)
					};
				true -> Acc
			end
		end,
		Namespaces, New ).
		
merge( Namespaces1, Namespaces2 ) ->
	#namespaces{
		prefixToUri = dict:merge( fun(_, V1, _) -> V1 end, Namespaces1#namespaces.prefixToUri, Namespaces2#namespaces.prefixToUri ),
		uriToPrefix = dict:merge( fun(_, V1, _) -> V1 end, Namespaces1#namespaces.uriToPrefix, Namespaces2#namespaces.uriToPrefix )
	}.
