-module (sort).

-export ([merge/2]).

merge( [], Ls2 ) ->
    Ls2;
merge( Ls1, [] ) ->
    Ls1;
merge( [H1|T1], [H2|T2] ) ->
    case H1 < H2 of
	true ->
	    [H1 | merge( T1, [H2|T2] )];
	false ->
	    [H2 | merge( [H1|T1], T2 )]
    end.