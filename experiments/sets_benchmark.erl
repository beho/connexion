-module (sets_benchmark).

-export ([bench/2, random_lists/2]).

bench( Count, ListsMaxSize ) ->
	{T, Lists} = timer:tc( ?MODULE, random_lists, [Count, ListsMaxSize] ),
	io:format( "generating: ~p ms ~p~n", [T, length(Lists)]),
	io:format("gb_sets: ~p ms~n", [benchmark_gb( Lists )] ),
	io:format("sets: ~p ms~n", [benchmark_sets( Lists )] ).

random_lists( Count, ListMaxSize ) ->
	X = [{random_list( random:uniform( ListMaxSize ) ), X} || X <- lists:seq(1, Count)],
	[L || {L, _} <- X].

random_list( Size ) ->
	L = [{get_random_string( random:uniform(64) + 15, "acdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ:/#_-" ), X} || X <- lists:seq(1, Size)],
	[S || {S, _} <- L].


get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).

benchmark_gb( Lists ) ->
    {TimeC, _} = timer:tc(?MODULE, gb_sets_create, [Lists]),
    TimeC.
     
gb_sets_create( Lists ) ->
	gb_sets:to_list( lists:foldr( fun gb_sets_combine/2, gb_sets:new(), Lists ) ).


gb_sets_combine( List, Set ) ->
	gb_sets:union( gb_sets:from_list( List ), Set ).


	
benchmark_sets( Lists ) ->
	{TimeC, _} = timer:tc(?MODULE, sets_create, [Lists]),
    TimeC.

sets_create( Lists ) ->
	sets:to_list( lists:foldr( fun sets_combine/2, sets:new(), Lists ) ).


sets_combine( List, Set ) ->
	sets:union( sets:from_list( List ), Set ).

