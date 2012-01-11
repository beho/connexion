% implementation of a skew heap specifically for plans
-module (plan_heap).

-compile(export_all).

-include ("querying.hrl").

-define (EMPTY_QUEUE, []).

new() ->
     ?EMPTY_QUEUE.

fromList( List ) ->
	lists:foldl( fun(P, Q) -> insert( P, Q ) end, new(), List ).

isEmpty( ?EMPTY_QUEUE ) ->
	true;
isEmpty( _ ) ->
	false.


insert( Tuple, ?EMPTY_QUEUE ) ->
	{Tuple, ?EMPTY_QUEUE, ?EMPTY_QUEUE};
insert( Tuple, Heap ) ->
	merge( {Tuple, ?EMPTY_QUEUE, ?EMPTY_QUEUE}, Heap ).

%out(Heap) -> {min(Heap), deleteMin(Heap)}.

takeSmallest( {{_, Data}, Left, Right} ) ->
	{Data, merge( Left, Right )}.

smallest( {{_, Data}, _, _} ) ->
	Data.

deleteMin( {_, Left, Right} ) ->
	merge( Left, Right).

merge( Left, ?EMPTY_QUEUE ) -> 
	Left;
merge( ?EMPTY_QUEUE, Right ) ->
	Right;
merge( {Ltuple, LL, LR}, {Rtuple, _, _} = Right ) when element(1, Ltuple) =< element(1, Rtuple) ->
     {Ltuple, LR, merge( LL, Right )};
merge( Left, {Rtuple, RL, RR} ) ->
	{Rtuple, RR, merge( RL, Left )}.

toList( Queue ) ->
	toList( Queue, [] ).

toList( ?EMPTY_QUEUE, List ) ->
	lists:reverse( List );
toList( {{_, Data}, Left, Right}, List ) ->
	toList( merge( Left, Right ), [Data|List] ).
	

%join( {Plan, Left, Right}, Heap) ->
%    {Plan, Right, merge( Left, Heap )}.