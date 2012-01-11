-module (benchmarks).
-export ([go/0]).

go() ->
	sys:statistics( concatenation, true ),
	[X || X <- lists:seq(1,10)] ++ [Y || Y <- lists:seq(1,1000)],
	sys:statistics( concatenation, false ),
	sys:statistics( concatenation, get ).