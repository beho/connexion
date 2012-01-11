-module (mergejoin).
-compile(export_all).
-export ([join/4, format/3]).

join( Left, Right, On, ResultFormat ) ->
	join( Left, Right, On, ResultFormat, [] ).
	
join( [], _Right, _On, _ResultFormat, Joined ) ->
	lists:reverse( Joined );
	
join( [L|_] = Left, [R|_] = Right, {Lidx, Ridx} = On, ResultFormat, Joined ) ->
	Lval = element(Lidx, L),
	{LSet, LRest} = lists:splitwith( fun(Tuple) -> Lval == element( Lidx, Tuple ) end, Left ),
	
	Rval = element(Ridx, R),
	{RSet, RRest} = lists:splitwith( fun(Tuple) -> Rval == element( Ridx, Tuple ) end, Right ),
	
	if
		Lval == Rval ->
			join( LRest, RRest, On, ResultFormat, [format( ResultFormat, A, B ) || A <- LSet, B <- RSet] ++ Joined );
		Lval > Rval ->
			join( Left, RRest, On, ResultFormat, Joined );
		Lval < Rval ->
			join( LRest, Right, On, ResultFormat, Joined )
	end.
	
format( FormatFuns, LTuple, RTuple ) ->
	List = lists:foldr( fun(F, Acc) -> [F( LTuple, RTuple )|Acc] end, [], FormatFuns ),
	list_to_tuple( List ).
	
test() -> [fun(L,R) -> element(1,L) end, fun(L,R)-> element(1,R) end].


		 