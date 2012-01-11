-module (evaluation_tree).

-compile(export_all).

-include ("querying.hrl").

codeUpdate() ->
	?MODULE:updated().
	
updated() ->
	true.



build( TriplePatterns ) ->
	% must get this from some cache, can't compute each time.
	Stats = model:statistics(),
	build( TriplePatterns, Stats ).
	
build( TriplePatterns, Stats ) ->
	TriplePatternPlans = lists:map( 
		fun(T) -> 
			Op = triple_pattern:opFor( T ),
			#plan{
				estimatedSize = heuristics:estimate( Op, Stats ),
				cost = 0,
				containedTriplePatterns = ordsets:from_list( [T] ),
				rootOp = Op } end,
		TriplePatterns ),
	
	ElementaryPlansDict = orddict:from_list( lists:zip( TriplePatterns, TriplePatternPlans ) ),
	AllPatternsSet = ordsets:from_list( TriplePatterns ),
	Count = ordsets:size( AllPatternsSet ),
	
	Open = plan_heap:fromList( lists:map( fun(P) -> {heuristics:remainingCost(P, Count), P} end, TriplePatternPlans ) ),
	% kill if anything has estimate 0 resp. if it is unusable( ?a ?b ?c, uri1 uri2 uri3 )

%	io:format("~p~n", [TriplePatternPlans]),
	
	aStar( {Open, 1, Count}, {{AllPatternsSet, Count}, ElementaryPlansDict}, Stats ).
	
	
		
evaluate( #plan{ rootOp = Op } ) ->
	Bindings = evaluate( Op ),
	erlang:garbage_collect(),
	Bindings;
	
evaluate( #operator{ inputs = [] } = Op ) ->
	io:format("evaluating no-input op: ~p~n", [Op]),
	triple_pattern:evaluate( Op );
evaluate( #operator{ type = OpType, inputs = Inputs } = Op ) ->
	io:format("evaluating input op: ~p~n", [Op]),
	EvaluatedInputs = lists:map( fun evaluate/1, Inputs),
	OpWithEvaluatedInputs = Op#operator{ inputs = EvaluatedInputs },

	OpType:evaluate( OpWithEvaluatedInputs ).
	

	
aStar( {Open, Step, Max}, {{AllPatterns, Count}, ElementaryPlans} = Input, Stats ) ->
	io:format( "===== A* step: ~p =====~n", [Step] ),
%	io:format("open list: ~p~n", [Open]),
	{First, OpenRest} = plan_heap:takeSmallest( Open ),
	OpenRestAsList = plan_heap:toList( OpenRest ),
	io:format("best: ~p~n", [First]),

	Expanded = expand( First, ordsets:subtract( AllPatterns, First#plan.containedTriplePatterns ), ElementaryPlans ),
%	io:format("expanded: ~p (~p)~n", [length(Expanded), Expanded]),
	Combined = combine( First, OpenRestAsList ),
%	io:format("combined: ~p (~p)~n", [length(Combined), Combined]),
	
	%NewPlans = Expanded ++ Combined ++ OpenRestAsList,
	NewPlans = eliminate( Expanded ++ Combined ++ OpenRestAsList ),
	NewPlansAsHeap = plan_heap:fromList( lists:map( fun(P) -> {heuristics:remainingCost(P, Count), P} end, NewPlans ) ),
	%io:format("new plans: ~p~n", [NewPlansAsHeap] ),
%	io:format("relevant: ~p (~p)~n", [length(NewPlans), NewPlans] ),
	
	Best = plan_heap:smallest( NewPlansAsHeap ),
	
	case ordsets:size( Best#plan.containedTriplePatterns ) =:= Count of
	 	true -> io:format("max open list length: ~p~n", [max(Max, length(NewPlans))]), Best;
		false -> aStar( {NewPlansAsHeap, Step + 1, max(Max, length(NewPlans))}, Input, Stats )
	end.

expand( Plan, TriplePatterns, ElementaryPlans ) when is_list(TriplePatterns) ->
	Plans = lists:foldl( fun(P, Acc) -> 
		joinPlans( Plan, orddict:fetch( P, ElementaryPlans ) ) ++ Acc end, [], TriplePatterns ),
		
%	io:format( "expaned: ~p~n", [Plans] ),
	Plans.
		
%	eliminate( Plans ).
		
combine( Plan, Plans ) ->
	CombinedPlans = lists:foldl( fun(P, Acc) ->
		joinPlans( Plan, P ) ++ Acc end, [], Plans ),
		
%	io:format( "combined: ~p~n", [CombinedPlans] ),
	CombinedPlans.
		
%	eliminate( CombinedPlans ).
	
joinPlans( Plan1, Plan2 ) ->
	lists:foldl( fun(Module, Acc) ->
	%	io:format("joining plans (~p, ~p): ~p~n ~p~n~n", [haveNoCommonTriplePattern( Plan1, Plan2 ), haveCommonVariables( Plan1, Plan2 ), Plan1, Plan2]),
		case haveNoCommonTriplePattern( Plan1, Plan2 ) andalso haveCommonVariables( Plan1, Plan2 ) of
			true -> 
				[Module:planForJoining(Plan1, Plan2), Module:planForJoining(Plan2, Plan1)] ++ Acc;
			false -> Acc
		end
	end, [], ?JOIN_MODULES ).
	
eliminate( Plans ) ->
	eliminate( Plans, Plans ).

eliminate( [], _ ) ->
	[];
eliminate( [P|T], Plans ) ->
	case worseThanAny( P, Plans ) of
		true -> eliminate( T, Plans );
		false -> [P|eliminate( T, Plans )]
	end.

worseThanAny( _Plan, [] ) ->
	false;
worseThanAny( #plan{ containedTriplePatterns = Patterns1, cost = Cost1, rootOp = Op1 } = Plan, 
		[#plan{ containedTriplePatterns = Patterns2, cost = Cost2, rootOp = Op2 }|T] ) ->
	case ordsets:is_subset( Patterns1, Patterns2 ) andalso Cost1 > Cost2 of
		true ->
			case differentFirstVarInVarOrder( Op1#operator.varOrder, Op2#operator.varOrder ) of
				true -> worseThanAny( Plan, T );
				false -> true
			end;
		false -> worseThanAny( Plan, T )
	end.
	
	
	
haveNoCommonTriplePattern( #plan{ containedTriplePatterns = T1 }, #plan{ containedTriplePatterns = T2 } ) ->
	ordsets:size( ordsets:intersection( T1, T2 ) ) =:= 0.
			
haveCommonVariables( #plan{ rootOp = #operator { varOrder = L } }, #plan{ rootOp = #operator{ varOrder = R } } ) ->
	ordsets:size( ordsets:union( ordsets:from_list(L), ordsets:from_list(R) ) ) =/= 0.

	
differentFirstVarInVarOrder( [V1|_], [V2|_] ) ->
	V1 =/= V2.
	
		
		
testOneVarJoin() ->
	build( [{1, rdf:type(), rdf:property()}, {1, rdfs:domain(), rdfs:resource()}] ).
	%PID ! {getNext, [], {self(), 1}}.

testCnxOneVar() ->
	build( [{1, rdf:type(), "http://www.fit.vutbr.cz/connexion#Album"}, {1, "http://www.fit.vutbr.cz/connexion#releaseDate", "2003"}, {1, "http://www.fit.vutbr.cz/connexion#label", "http://www.fit.vutbr.cz/connexion#NinjaTune"}] ).
	%PID ! {getNext, [], {self(), 1}}.

testCnx() ->
	build( [{1, rdf:type(), 2}, {1, "http://www.fit.vutbr.cz/connexion#releaseDate", 3}, {1, "http://www.fit.vutbr.cz/connexion#label", "http://www.fit.vutbr.cz/connexion#NinjaTune"}] ).

