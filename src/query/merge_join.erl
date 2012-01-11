-module (merge_join).

-export ([
%	nodes/2,
	planForJoining/2,
	opForJoining/2,
	estimateCost/3]).
	% ensureIdenticalSortOrder/2]).
-compile(export_all).

%-include ("actor.hrl").

-include ("querying.hrl").

%-record (join_state, {left, right, joinSpec, varOrder, rightRest = gb_sets:new()}).

codeUpdate() ->
	?MODULE:updated().
	
updated() ->
	true.
	
	
evaluate( #operator{ type = ?MERGE_JOIN_OP, varOrder= VarOrder, specific = #join_spec{ sort = {SortType, SortKey}} = JoinSpec, inputs = [L, R] } = Op ) ->
	{Lsorted, Rsorted} = case SortType of
		nosort -> {L#bindings.tuples, R#bindings.tuples};
		sort -> {sortTuples( L#bindings.tuples, SortKey ), R#bindings.tuples};
		sortboth -> {LsortKey, RsortKey} = SortKey, {sortTuples( L#bindings.tuples, LsortKey ), sortTuples( R#bindings.tuples, RsortKey )}
	end,
	
	#bindings{ varOrder = VarOrder, tuples = join( Lsorted, Rsorted, JoinSpec ) }.
%	Op#operator{ tuples = join( Lsorted, Rsorted, JoinSpec ) }.
	
planForJoining( Plan1, Plan2 ) ->
%	io:format("joining ~p~n~p~n", [Plan1, Plan2]),
	Op = opForJoining( Plan1#plan.rootOp, Plan2#plan.rootOp ),
	#plan{
		cost = estimateCost( Plan1, Plan2, Op#operator.specific#join_spec.sort ),
		% take bigger input's size as result's size. Does not make much sense. Need to figure out better way. 
		estimatedSize = heuristics:estimateJoinSize( Plan1, Plan2 ),
		rootOp = Op,
		containedTriplePatterns = ordsets:union( Plan1#plan.containedTriplePatterns, Plan2#plan.containedTriplePatterns )
	}.
	
opForJoining( #operator{ varOrder = Lkey } = L, #operator{ varOrder = Rkey } = R ) ->
	LvarInfos = varOrderToVarInfos( Lkey ),
	RvarInfos = varOrderToVarInfos( Rkey ),

	{JoinVars, JoinIdxs} = lists:foldr( 
		fun({Var, LIdx, RIdx}, {JoinVarsAcc, {LAcc, RAcc}}) -> {[Var|JoinVarsAcc], {[LIdx|LAcc], [RIdx|RAcc]}} end, {[], {[], []}},
		commonVariablePositionPairs( LvarInfos, RvarInfos ) ),

	% JoinVarsOrderInLkey = lists:filter( fun(V) -> lists:member( V, JoinVars ) end, Lkey ),
	% JoinVarsOrderInRkey = lists:filter( fun(V) -> lists:member( V, JoinVars ) end, Rkey ),
	
	Sort = sortSpec( LvarInfos, RvarInfos, JoinVars ),

	LvarDict = varDict( LvarInfos, l ),
	RvarDict = varDict( RvarInfos, r ),
	

	Diff = lists:subtract( Rkey, Lkey ),
	{VarOrder, ResultFormat} = joinOutputSpec( Lkey, LvarDict, Diff, RvarDict ),


	JoinSpec = #join_spec{ 
		joinIdxs = JoinIdxs,
		resultFormat = ResultFormat,
		sort = Sort },
		
	#operator{
		type = ?MERGE_JOIN_OP,
		varOrder = VarOrder,
		inputs = [L, R],

		specific = JoinSpec
	}.
	
sortSpec( LvarInfos, RvarInfos, JoinVars ) ->
	JoinVarsSet = ordsets:from_list( JoinVars ),
	Lprefix = prefixWithJoinVars( LvarInfos, JoinVarsSet ),
	Rprefix = prefixWithJoinVars( RvarInfos, JoinVarsSet ),
	
	Lvars = vars( LvarInfos ),

	LcontainsLPrefix = lists:foldl( fun({V, _}, Acc) -> lists:member( V, Lvars ) and Acc end, true, Rprefix ),
%	io:format("~p ~p ~p~n", [Rprefix, Lvars, LcontainsLPrefix]),
	
	MapVarsToIdxs = fun(Vars, VarDict) ->
		lists:map( fun(V) -> orddict:fetch( V, VarDict ) end, Vars )
	end,
	
	if
		Lprefix == Rprefix -> {nosort, []};
		LcontainsLPrefix ->
			RprefixVars = vars( Rprefix ),
			{sort, MapVarsToIdxs( RprefixVars, varInfosToDict( LvarInfos ) ) ++ indexes(lists:filter(fun({V, _}) -> not lists:member(V, RprefixVars) end, LvarInfos ) )};
		true ->
			LjoinIdxs = MapVarsToIdxs( JoinVars, varInfosToDict( LvarInfos ) ),
			RjoinIdxs = MapVarsToIdxs( JoinVars, varInfosToDict (RvarInfos ) ),
			LnonJoin = lists:filter(fun({V, _}) -> not lists:member(V, JoinVars) end, LvarInfos ),
			RnonJoin = lists:filter(fun({V, _}) -> not lists:member(V, JoinVars) end, RvarInfos ),
			{sortboth, {LjoinIdxs ++ indexes( LnonJoin ), RjoinIdxs ++ indexes( RnonJoin )}}
	end.
	
	
joinOutputSpec( PrefixVarOrder, PrefixVarDict, SuffixVarOrder, SuffixVarDict ) ->
	ConstructFun = fun(VarOrder, VarDict, {_InitVarOrderAcc, _InitResultFormatAcc} = InitAcc) ->
		lists:foldl( fun(Var, {VarOrderAcc, FormatAcc}) -> 
			{[Var|VarOrderAcc], [orddict:fetch( Var, VarDict )|FormatAcc]} end, InitAcc, VarOrder )
	end,

	{RevVarOrder, RevFormat} = ConstructFun( SuffixVarOrder, SuffixVarDict, 
		ConstructFun( PrefixVarOrder, PrefixVarDict, {[], []} ) ),

	{lists:reverse( RevVarOrder ), lists:reverse( RevFormat )}.


% computed as cost of sort (always sorting L - expecting it to be smaller)
% 
estimateCost( #plan{ estimatedSize = Lsize }, #plan{ estimatedSize = Rsize }, Sort ) ->
	SortCost = 
		case Sort of
			{nosort, _} -> 0;
			{sort, _} -> 
				case Lsize of
					0 -> 0;
					_ -> Lsize * heuristics:log2( Lsize )
				end;
			{sortboth, _} ->
				LsortCost = case Lsize of
					0 -> 0;
					_ -> Lsize * heuristics:log2( Lsize )
				end,
				RsortCost = case Rsize of
					0 -> 0;
					_ -> Rsize * heuristics:log2( Rsize )
				end,
				LsortCost + RsortCost
		end,
	JoinCost = Lsize + Rsize,
	round( SortCost + JoinCost ).

% accept resultVarOrder a ResultSortKey as parameter, so do final sorting so next node can directly start joining if so

% nodes( {_LNode, LVarOrder} = L, {_RNode, LVarOrder} = R ) ->
% 	new( [L, R, opForJoining( LVarOrder, LVarOrder )] ).

% %% @doc Initializes join actor. Analogous to open function of standard iterator interface
% initialize( _Name, [Left, Right, JoinIdxs, ResultFormat, VarOrder] ) -> 
% %	JoinConditions = pairsToEqual( LVarInfos, RVarInfos ),
% %	{VarInfos, OutputFields} = varPositionsSet( LVarInfos, RVarInfos ),
% 	% send var infos ? or somehow return to caller.
% 	#join_state{ left = Left, right = Right, varOrder = VarOrder, joinSpec = #join_spec{ joinIdxs = JoinIdxs, resultFormat = ResultFormat }}.
% 
% %% Analogous to close of standard iterator interface.
% stop( #join_state{ left = {LeftNode, _}, right = {RightNode, _} } ) ->
% 	LeftNode ! stop,
% 	RightNode ! stop.
% 	
% 
% methodFor( Selector ) ->
% 	case Selector of
% 		?M( getNext );
% 		
% 		?DNU_HANDLER
% 	end.
% 
% 	
% getNext( _Count, {Receiver, QID}, 
% 	#join_state{ left = {LeftNode, _}, right = {RightNode, _}, varOrder = VarOrder, joinSpec = JoinSpec, rightRest = RightRest} = Join ) ->
% 			
% 	LQID = make_ref(),
% 	RQID = make_ref(),
% 	%io:format("executing right~n"),
% 	RightNode ! {execute, [], {self(), RQID}},
% 	%io:format("get next left~n"),
% 	LeftNode ! {getNext, [], {self(), LQID}},
% 	
% 	receive
% 		{RQID, ok} -> RightNode ! {getNext, [], {self(), RQID}}
% 	end,
% 	
% 	LSet =
% 		receive
% 			{LQID, {_, S1}} -> S1
% 		end,
% 		
% 	RSet =
% 		receive
% 			{RQID, {_, S2}} -> S2
% 		end,
% 	
% 	%{Merge, Rest} = 
% 	io:format( "LSet: ~p~nRSet: ~p~n", [LSet, RSet] ),
% 	Joined = join( LSet, RSet, JoinSpec ),
% 	io:format("joined: ~p~n", [Joined] ),
% 	Receiver ! {QID, {VarOrder, Joined}},
% 	Rest = gb_sets:empty(),
% 	
% 	{ok, Join#join_state{ rightRest = gb_sets:union( RightRest, Rest )}}.
	

sortTuples( Tuples, SortKey ) ->
	lists:sort( fun(A, B) -> isLessThanOrEqual( A, B, SortKey ) end, Tuples ).
	
isLessThanOrEqual( _, _, [] ) ->
	true;
isLessThanOrEqual( Ltuple, Rtuple, [Idx|Idxs] ) ->
	Lvalue = element( Idx, Ltuple ),
	Rvalue = element( Idx, Rtuple ),
	if
		Lvalue == Rvalue -> isLessThanOrEqual( Ltuple, Rtuple, Idxs );
		Lvalue < Rvalue -> true;
		true -> false % Lvalue > Rvalue
	end.	

% join:join([{a,1},{b,2},{c,3}], [{a,x}, {a,y}, {b,z}], {[1], [1]}, [{l,2}, {r,2}]).

% MUST ENSURE SORT ORDER FOR VARIABLES TO JOIN ON.

join( Left, Right, JoinSpec ) when is_list(Left) and is_list(Right) ->
	join( {Left, undefined}, {Right, undefined}, JoinSpec );
join( #operator{tuples = Ltuples, condensedIdx = LcondIdx}, #operator{tuples = Rtuples, condensedIdx = RcondIdx}, JoinSpec ) ->
	join( {Ltuples, LcondIdx}, {Rtuples, RcondIdx}, JoinSpec );
join( {_, _} = Left, {_, _} = Right, JoinSpec ) ->
	join( Left, Right, JoinSpec, [] ).

join( {[], _}, _Right, #join_spec{}, Joined ) ->
	lists:reverse( Joined );
join( _Left, {[], _}, #join_spec{}, Joined ) ->
	lists:reverse( Joined );

join( {Ltuples, LcondensedIdx} = L, {Rtuples, RcondensedIdx} = R,
       #join_spec{joinIdxs = {LjoinIdxs, RjoinIdxs}, resultFormat = ResultFormat} = JoinSpec, Joined ) ->
	
	{LjoinVals, Lequal, Lrest} = splitByEqualToFirstOnIdxs( Ltuples, LjoinIdxs, LcondensedIdx ),
	%io:format( "Ltuples: ~p; LjoinVals: ~p, Lequal: ~p, Lrest: ~p~n", [Ltuples, LjoinVals, Lequal, Lrest]),
	{RjoinVals, Requal, Rrest} = splitByEqualToFirstOnIdxs( Rtuples, RjoinIdxs, RcondensedIdx ),
	%io:format( "Rtuples: ~p; RjoinVals: ~p, Requal: ~p, Rrest: ~p~n~n", [Rtuples, RjoinVals, Requal, Rrest]),
	
	if
		% in case of condensed tuples if there's no tuple in either relation, make finishing call.
		(LjoinVals == undefined) or (RjoinVals == undefined) -> 
			join( {[], LcondensedIdx }, R, JoinSpec, Joined );
			
		LjoinVals == RjoinVals ->
			join( {Lrest, LcondensedIdx}, {Rrest, RcondensedIdx}, 
				JoinSpec, [format( ResultFormat, A, B ) || A <- Lequal, B <- Requal] ++ Joined );
				
		LjoinVals > RjoinVals ->
			join( L, {Rrest, RcondensedIdx }, JoinSpec, Joined );
			
		LjoinVals < RjoinVals ->
			join( {Lrest, LcondensedIdx}, R, JoinSpec, Joined )
	end.

splitByEqualToFirstOnIdxs( [], _Idxs, undefined ) ->
	{undefined, [], []};
splitByEqualToFirstOnIdxs( [H|_] = Tuples, Idxs, undefined ) ->
	ValuesOfFirst = values( H, Idxs ),
	{Equal, NotEqual} = splitByEqualOnIdxs( Tuples, Idxs, ValuesOfFirst ),
	%lists:splitwith( fun(Tuple) -> values( Tuple, Idxs ) == ValuesOfFirst end, Tuples ),
	{ValuesOfFirst, Equal, NotEqual};

% This one is for condensed tuples	
splitByEqualToFirstOnIdxs( Tuples, Idxs, CondensedIdx ) ->
	case nextTuple( Tuples, CondensedIdx ) of
		{H, _} -> 
			ValuesOfFirst = values( H, Idxs ),
			{Equal, NotEqual} = splitCondensedByEqualOnIdxs( {Tuples, CondensedIdx}, Idxs, ValuesOfFirst ),
			{ValuesOfFirst, Equal, NotEqual};
		undefined -> {undefined, [], []}
	end.
	
splitByEqualOnIdxs( Tuples, Idxs, Vals ) when is_list( Tuples ) ->
	splitByEqualOnIdxs( Tuples, Idxs, Vals, [] ).

splitByEqualOnIdxs( [], _, _, Equal ) ->
    {Equal, []};
splitByEqualOnIdxs( [H|T], Idxs, Vals, Equal ) ->
    case values( H, Idxs ) == Vals of
		true -> splitByEqualOnIdxs( T, Idxs, Vals, [H|Equal] );
		false -> {Equal, [H|T]}
    end.
		
	
splitCondensedByEqualOnIdxs( {_Tuples, _CondensedIdx } = TupleSpec, Idxs, Vals ) ->
	splitCondensedByEqualOnIdxs( TupleSpec, Idxs, Vals, [] ).
	
splitCondensedByEqualOnIdxs( {Tuples, CondensedIdx}, Idxs, Vals, Equal ) ->
	case nextTuple( Tuples, CondensedIdx ) of
		{First, Rest} -> 
			case values( First, Idxs ) == Vals of
				true  -> splitCondensedByEqualOnIdxs( {Rest, CondensedIdx}, Idxs, Vals, [First|Equal] );
				false -> {Equal, Tuples}
			end;
		undefined -> {Equal, []}
	end.
	
		
nextTuple( [], _ ) ->
	undefined;
nextTuple( [H|T], CondensedIdx ) ->
	case element( CondensedIdx, H ) of
		[] -> nextTuple( T, CondensedIdx );
		[HCondensed|TCondensed] -> 
			Values = lists:foldr( 
				fun(Idx, Current) ->
					case Idx == CondensedIdx of
						true  -> [HCondensed|Current];
						false -> [element( Idx, H )|Current]
					end
				end,
				[], lists:seq( 1, tuple_size(H) ) ),
					
			{list_to_tuple(Values), [setelement( CondensedIdx, H, TCondensed )|T]}
	end.


	
commonVariablePositionPairs( LVarInfos, RVarInfos ) ->
	join( {ordsets:from_list( LVarInfos ), undefined}, {ordsets:from_list( RVarInfos ), undefined},
		#join_spec{ joinIdxs = {[1], [1]}, resultFormat = [{l, 1}, {l, 2}, {r, 2}] } ).

%indexesFor( ResultStructure ) ->
%	lists:seq( 1, length( ResultStructure ) ).
varOrderToVarInfos( VarOrder ) ->
	lists:zip( VarOrder, lists:seq( 1, length( VarOrder ) ) ).
%	lists:foldl( fun({V, Idx}, Dict) -> orddict:store( V, Idx, Dict ) end, orddict:new(), 
%		lists:zip( VarOrder, lists:seq( 1, length( VarOrder ) ) ) ).

varInfosToDict( VarInfos ) ->
	lists:foldl( fun({V, Idx}, Dict) -> orddict:store( V, Idx, Dict ) end, orddict:new(), VarInfos ).
	
indexes( VarInfos ) ->
	lists:map( fun({_V, Idx}) -> Idx end, VarInfos ).
	
vars( VarInfos ) ->
	lists:map( fun({V, _Idx}) -> V end, VarInfos ).
	
	

values( Tuple, Idxs ) ->
	lists:foldr( fun(Idx, Acc) -> [value( Tuple, Idx )|Acc] end, [], Idxs ).

	
value( Tuple, Idx ) when is_tuple( Tuple ) ->
	element( Idx, Tuple );

value( AtomicValue, _ ) ->
	AtomicValue.



format( FieldIds, LTuple, RTuple ) ->
	List = lists:foldr( fun(FieldId, Acc) -> [getField( LTuple, RTuple, FieldId )|Acc] end, [], FieldIds ),
	list_to_tuple( List ).
	
getField( L, R, {Which, Idx} ) ->
	case Which of
		l -> value( L, Idx );
		r -> value( R, Idx )
	end.
	
prefixWithJoinVars( VarInfos, JoinVars ) ->
	prefixWithJoinVars( VarInfos, JoinVars, [] ).
	
prefixWithJoinVars( _, [], Acc ) ->
	lists:reverse( Acc );
prefixWithJoinVars( [{V, _} = VarInfo|Vs], JoinVars, Acc ) ->
	prefixWithJoinVars( Vs, ordsets:del_element( V, JoinVars ), [VarInfo|Acc] ).
	
	

% haveSamePrefixWithJoinVars( _, _, [] ) ->
% 	true;
% haveSamePrefixWithJoinVars( [L|Ls], [R|Rs], JoinVarsSet ) when L =:= R ->
% 	haveSamePrefixWithJoinVars( Ls, Rs, ordsets:del_element( L, JoinVarsSet) );
% haveSamePrefixWithJoinVars( [L|_], [R|_], _ ) when L =/= R ->
% 	false.

	
varDict( VarInfos, RelationId ) ->
	lists:foldl(
		fun({Var, Position}, Acc) -> orddict:store( Var, {RelationId, Position}, Acc ) end,
		orddict:new(), VarInfos ).
		

	

	
% hasCompatibleSortKey( LresultSetSpec, RresultSetSpec ) ->
% 	LsortKeyAsVars = sortKeyAsVars( LresultSetSpec ),
% 	RsortKeyAsVars = sortKeyAsVars( RresultSetSpec ),
% 	%io:format("l: ~p r:~p~n", [LsortKeyAsVars, RsortKeyAsVars]),
% 	lists:prefix( LsortKeyAsVars, RsortKeyAsVars ) 
% 		orelse lists:prefix( RsortKeyAsVars, LsortKeyAsVars ).
	
%sortKeyAsVars( #operator{ varOrder = VarOrder, sortKey = SortKey} ) ->
%	VarOrderTuple = list_to_tuple( VarOrder ),
%	lists:foldr( fun(Idx, Acc) -> [element( Idx, VarOrderTuple )|Acc] end, [], SortKey ).
	
	
% ensureIdenticalSortOrder( Lbindings, Rbindings ) ->
% 	{Lbindings, Rbindings}.


test() ->
	 join:join(
		#operator{tuples=[{a, [1,2,3]}, {b, [4,5]}], condensedIdx = 2 },
		#operator{tuples=[{[a,b], 6}], condensedIdx=1 }, 
		#join_spec{ joinIdxs = {[1],[1]}, resultFormat = [{l,2}, {r,2}] } ).
	
test1() ->
	join:join({[{a,1},{b,2},{b,3}], undefined}, {[{1,x}, {1,y},{2,z}], undefined}, 
		#join_spec{ joinIdxs = {[2], [1]}, resultFormat = [{l, 1}, {r, 2}] } ).