-module (heuristics).

-export ([
	log2/1,
	tripleSelectivity/2,
	estimate/2,
	estimateJoinSize/2,
	estimateJoinSize/3,
	remainingCost/2
	 ]).

-include ("stats.hrl").
-include ("querying.hrl").

-include ("vocabularies/rdf.hrl").
-include ("vocabularies/rdfs.hrl").



-define (LOG2, 0.6931471805599453).

log2( X ) -> math:log( X ) / ?LOG2.



% joining triple_pattern_specs
%estimateJoinVariants( L, R ) ->
%	lists:map( fun(Mod) -> {Mod, Mod:estimateCost( L, R )} end, ?JOIN_MODULES ).
estimateJoinSize( Plan1, Plan2 ) ->
	estimateJoinSize( Plan1, Plan2, [] ).
estimateJoinSize( #plan{ estimatedSize = L }, #plan{ estimatedSize = R }, _Stats ) ->
	max( L, R ).
	
% follows more or less M. Stocker et.al: SPARQL Basic Graph Pattern Optimization using Selectivity Estimation; 2008
estimate( #operator{ type = ?TRIPLE_PATTERN_OP, specific = Spec }, Stats ) ->
	triplePatternEstimate( Spec#triple_pattern_spec.pattern, Stats ).
	
remainingCost( #plan{ containedTriplePatterns = Patterns}, AllPatternsCount ) ->
	(AllPatternsCount - ordsets:size(Patterns)) * 100.
	

triplePatternEstimate( {S, P, O}, Stats ) ->
	case P of
		?rdf?type -> typeEstimate( S, O, Stats );
		P ->
			SE = subjectEstimate( S, Stats ),
			PE = propertyEstimate( P, Stats ),
			OE = objectEstimate( O, P, Stats ),
			io:format("S: ~p P: ~p O: ~p~n", [SE, PE, OE]),
			round(SE * PE * OE)
	end.
%	ResultSetSpec = Spec#triple_pattern_spec.resultSetSpec,
%	Estimate = ResultSetSpec#result_set_spec.estimate.
	
typeEstimate( S, O, #stats{ properties = Properties } ) when ?IS_VAR(S) andalso ?IS_VAR(O) ->
	case dict:find( ?rdf?type, Properties ) of
		error -> 0;
		{ok, #property_stats{ pairsCount = Pairs}} -> Pairs
	end;
typeEstimate( S, O, #stats{ classes = Classes } ) when ?IS_VAR(S) ->
	case dict:find( O, Classes ) of
		error -> 0;
		{ok, #class_stats{ membersCount = MembersCount} } -> MembersCount
	end;
typeEstimate( _S, O, #stats{ properties = Properties } ) when ?IS_VAR(O) ->
	case dict:find( ?rdf?type, Properties ) of
		error -> 0;
		{ok, #property_stats{ avgObjectOccurence = Avg}} -> Avg
	end. 
	
tripleSelectivity( {S, P, O}, Stats ) ->
	SS = subjectSelectivity( S, Stats ),
	PS = propertySelectivity( P, Stats ),
	OS = objectSelectivity( P, O, Stats ),
	io:format( "~p ~p ~p~n", [SS, PS, OS]),
	SS * PS * OS.
	

subjectSelectivity( S, _ ) when ?IS_VAR(S) ->
	1;
subjectSelectivity( _S, #stats{ classes = Classes } ) ->
	Stats = dict:fetch( ?rdfs?Resource, Classes ),
	1 / Stats#class_stats.membersCount.
	
propertySelectivity( P, _ ) when ?IS_VAR(P) ->
	1;
propertySelectivity( P, #stats{ triplesCount = T, properties = Properties } ) ->
	Stats = dict:fetch( P, Properties ),
	Stats#property_stats.pairsCount / T.
	

objectSelectivity( _P, O, _ ) when ?IS_VAR(O) ->
	1;
objectSelectivity( P, _O, #stats{ properties = Properties }) when ?IS_VAR(P) ->
	dict:fold( fun(#property_stats{ avgObjectOccurence = Avg}, Acc) -> Acc + Avg end, 0, Properties );
objectSelectivity( P, _O, #stats{ properties = Properties } ) ->
	Stats = dict:fetch( P, Properties),
	Stats#property_stats.avgObjectOccurence.



subjectEstimate( Subject, #stats{ classes = Classes } ) when ?IS_VAR( Subject ) ->
	case dict:find( ?rdfs?Resource, Classes ) of
		error -> 0;
		{ok, #class_stats{ membersCount = Count }} -> Count
	end;
subjectEstimate( _Subject, _Stats ) ->
	1.
%	case dict:find( ?'rdfs:Resource', Classes ) of
%		error -> 0; % this really should not happen. rdfs:Resource must be there after connexion:setup().
%		{ok, #class_stats{ membersCount = Count }} -> 1 / Count
%	end.
	
% refinement for type !
propertyEstimate( ?rdf?type, _Stats ) ->
	1;
propertyEstimate( Property, #stats{ triplesCount = Count } ) when ?IS_VAR( Property ) ->
	Count;
propertyEstimate( Property, #stats{ properties = Properties } ) ->
	io:format("~p~n", [Property]),
	case dict:find( Property, Properties ) of
		error -> 0; % if predicate is not known
		{ok, #property_stats{ pairsCount = Count }} -> Count
	end.

objectEstimate( Object, Property, _ ) when ?IS_VAR( Property ) andalso ?IS_VAR( Object ) ->
	1;
objectEstimate( _Object, Property, Stats ) when ?IS_VAR( Property ) ->
	Stats#stats.avgObjectOccurence;
objectEstimate( Object, Property, #stats{ properties = Properties } ) when ?IS_VAR( Object ) ->
	case dict:find( Property, Properties ) of
		error -> 0;
		{ok, #property_stats{ avgObjectOccurence = Avg }} -> Avg
	end;
objectEstimate( _Object, _Properties, _ ) ->
	1.
	

	

	