%-define (VAR_MARKER, '_').

% variants of joining
-define (JOIN_MODULES, [
	merge_join ]).

% because only built-in functions and operators can be used in guards, macro
% has to be used for that purpose if ugly code is to be avoided.
-define (IS_VAR (V), is_integer(V) ).

isVar( Term ) ->
	is_integer( Term ).
	
% -record (result_set_spec, {
% 	varOrder, % order of variable bindings in tuple
% 	sortKey, % list of indexes which is used as a sort key for list of tuples
% 	condensedIdx = undefined, % if produced result will use condensation, this specifies its index
% 	estimate = 0 }). % selectivity of a result set relatively to whole knowledge base
% 
% 
% -record (triple_pattern_spec, {
% 	nature = normal, % dump / useless
% 	resultSetSpec, % specification of tuples in resulting binding 
% 	pattern, % pattern itself
% 	evaluationFun }). % fun that will return result tuples. use evaluate(tripl_pattern_spec) to obtain bindings 
% 	
-record (pattern_group_spec, {
	resultSetSpec,
	triplePatterns, % list of triple patterns
	inputs = [] }). % nodes which tuple sets have to be joined with pattern group somewhere along the way. 

% -record (bindings, {
% 	resultSetSpec, % specification of tuples
% 	tuples = undefined }). %
	
% -record (join_spec, {
% 	resultVarOrder, 
% 	joinIdxs, 
% 	resultFormat }).
	
-define (TRIPLE_PATTERN_OP, triple_pattern).
-define (MERGE_JOIN_OP, merge_join).
-define (SELECT_OP, select).

-record (plan, {
	estimatedSize,
	cost,
	containedTriplePatterns,
	rootOp }).
	
-record (bindings, {
	varOrder,
	tuples }).
	
-record (operator, {
	type, % triplePattern, join, select, ..
	varOrder, % represents sortKey at the same time
%	sortKey,
	tuples = undefined,
	condensedIdx = undefined,
%	estimatedSize = 0,
	inputs = [],
	specific }).
	
-record (triple_pattern_spec, {
	pattern,
	evaluationFun }).
	
-record (join_spec, {
	joinIdxs,
	sort,
	resultFormat}).
	