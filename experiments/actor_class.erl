-module (actor_class).

-include ("actor.hrl").

-include ("../src/definitions.hrl").

%-export ([initialize/1, method_for/1, start/0]).
%-export ([add/3, contains/3, individuals/2, size/2]).

%new( Args ) ->
%	?BASE_MODULE:new( ?MODULE, Args ).

initialize( Name ) ->
	io:format("initialize~n", []),
	#class{ label = Name, individuals = ets:new( list_to_atom(atom_to_list(Name) ++ "_data"), [ordered_set] ) }.
	
method_for( Selector ) ->
	io:format("method_for~n"),
	case Selector of
		add -> fun add/3;
		contains -> fun contains/3;
		individuals -> fun individuals/2;
		size -> fun size/2
	end.

add( URL, {From, QID}, ClassInfo = #class{ individuals = Inds } ) ->
	ets:insert( URL, Inds ),
	From ! {QID,ok},
	ClassInfo.
	
contains( URL, {From, QID}, ClassInfo = #class{ individuals = Inds } ) ->
	From ! {QID, ets:lookup( Inds, URL)},
	ClassInfo.
	
individuals( {From, QID}, ClassInfo = #class{ individuals = Inds } ) ->
	From ! {QID, ets:tab2list( Inds )},
	ClassInfo.
	
size( {From, QID}, ClassInfo = #class{ individuals = Inds } ) ->
	{size, Size} = lists:keyfind(size, 1, ets:info( Inds ) ),
	From ! {QID, Size},
	ClassInfo.
	