equalContent( List1, List2 ) ->
	lists:foldl( fun(E, Acc) -> lists:member(E, List1) and Acc end, true, List2 ) and (length( List1 ) == length( List2 )).