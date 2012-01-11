%% @doc Counts number of target nodes to be able to collect all responses
%% deterministically using deterministically in interactor.
toBeActivatedCount( SetsSelectorPairList ) ->
	lists:foldl( fun(Size, Sum) -> Sum + Size end, 0, 
		lists:map( fun({Set, _}) -> sets:size( Set ) end, SetsSelectorPairList ) ).

%% @doc Sends specific messages to specific sets of nodes.
spreadActivation( SetSelectorPairsList, Args, ID ) ->
	%io:format( "spreadActivation ~w~n", [SetsSelectorPairsList]),
	length( lists:map( fun({Set, Selector}) -> sendToAll( Set, Selector, Args, ID ) end, SetSelectorPairsList ) ).
	
spreadActivation( ActorSelectorArgsList, ID ) ->
	length( lists:map( fun({Actor, Selector, Args}) -> Actor ! {Selector, Args, ID} end, ActorSelectorArgsList ) ).
	
	
sendToAll( List, Selector, Args, ID = {_Receiver, _QID} ) when is_list(List) ->
	length( lists:map( fun(Element) -> 
		%io:format("~n~p ~p ~p ~p", [Element, Selector, Args, ID]),
		to_atom(Element) ! {Selector, Args, ID} end, List ) );
%% @doc Simple broadcast of a message to all nodes in a given set.
sendToAll( Set, Selector, Args, ID = {_Receiver, _QID} ) ->
	sendToAll( sets:to_list( Set ), Selector, Args, ID ).
	
	
	

sendMessageAndFoldResponses( Actor, Selector, Args, Fun, TransformFun, InitAcc ) when is_atom( Actor )->
	QID = make_ref(),
	Actor ! {Selector, Args, {self(), QID}},
	foldResponses( QID, Fun, TransformFun, InitAcc, 1 ).

foldResponses( _, _, TransformFun, Acc, 0 ) ->
	TransformFun( Acc );
foldResponses( QID, Fun, TransformFun, Acc, Remaining ) ->
	receive
		{QID, {Value, ChildrenCount}} ->
			%io:format("~w ~w", [Value, ChildrenCount]),
			%io:format( "~p ~p", [Value, Acc]),
			NewAcc = Fun( Value, Acc ),
			NewRemaining = Remaining - 1 + ChildrenCount,
			foldResponses( QID, Fun, TransformFun, NewAcc, NewRemaining )
	end.




sendMessageAndFoldSetResponses( Actor, Selector, Args ) when is_atom( Actor )->
%	io:format("~p ~p ~p~n", [Actor, Selector, Args]),
	QID = make_ref(),
	Actor ! {Selector, Args, {self(), QID}},
	foldSetResponses( QID ).

foldSetResponses( QID ) ->
	foldResponses( QID, fun foldUnion/2, fun foldSetToList/1, foldEmptySet(), 1 ).


%% @TODO should be as general as foldResponses ?
foldResponsesWithConfirmation( QID, Fun, Decider ) ->
	foldResponsesWithConfirmation( QID, Fun, Decider, false, 1 ).

foldResponsesWithConfirmation( _, _, _, Acc, 0 ) ->
	Acc;
foldResponsesWithConfirmation( QID, Fun, Decider, Acc, Remaining ) ->
	receive
		{QID, {Value, ChildrenCount}} ->
			NewAcc = Fun( Value, Acc ),
			NewRemaining = Remaining - 1 + ChildrenCount,
			foldResponsesWithConfirmation( QID, Fun, Decider, NewAcc, NewRemaining );
		{continue, {From, QID}} ->
			case Decider( Acc ) of
				true -> 
					From ! {QID, true},
					foldResponsesWithConfirmation( QID, Fun, Decider, Acc, Remaining );
				false ->
					From ! {QID, false},
					foldResponsesWithConfirmation( QID, Fun, Decider, Acc, Remaining - 1 )
			end
	end.

foldEmptySet() -> gb_sets:new().

foldUnion( List, Set ) ->
	gb_sets:union( gb_sets:from_list( List ), Set ).

foldSetToList( Set ) ->
	gb_sets:to_list( Set ).


foldOr( Bool1, Bool2 ) ->
	Bool1 or Bool2.

identity( E ) -> E.



to_atom( S ) when is_atom( S ) ->
	S;
to_atom( S ) when is_list( S ) ->
	list_to_atom( S ).

to_list( S ) when is_list( S ) ->
	S;
to_list( S ) when is_atom( S ) ->
	atom_to_list( S ).

