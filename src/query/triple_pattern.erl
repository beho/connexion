-module (triple_pattern).
-compile(export_all).
-export ([
	%actorFor/1,
	%execute/3
%	estimatedOpFor/2,
	opFor/1,
	evaluate/1 ]).

%-include ("actor.hrl").

-include ("messaging.hrl").
-include ("querying.hrl").
-include ("vocabularies/rdf.hrl").
-include ("vocabularies/rdfs.hrl").

%for( S, P, O ) ->
%	new( [{S, P, O}] ).

%actorFor( {Pattern, VarInfos} ) ->
%	ResultStructure = 
%		lists:map( 
%			fun({Var, _}) -> Var end,
%			lists:sort( fun({_VarA, PosA}, {_VarB, PosB}) -> PosA < PosB end, VarInfos ) 
%		),
	
%	{new( [Pattern, ResultStructure] ), ResultStructure}.

%initialize( _Name, [Pattern, ResultStructure] ) ->
%	{Pattern, {ResultStructure, undefined}}.
	
%stop( _ ) ->
%	ok.
	
%methodFor( Selector ) ->
%	case Selector of
%		?M( execute );
%		?M( getNext );
		
%		?DNU_HANDLER
%	end.
	
%execute( _, {Receiver, QID}, {{S, P, O} = Pattern, {Structure, _}} ) ->
%	Receiver ! {QID, ok},
%	io:format( "EXECUTE pre~n" ),
%	NewResult = opFor( S, P, O ),
	
%	io:format( "EXECUTE post~n" ),
%	{ok, {Pattern, {Structure, NewResult}}}.
	
%getNext( _, {Receiver, QID}, {{S, P, O} = Pattern, {Structure, Result}}) ->
%	io:format("Sending result (~p)~n", [Result] ),
%	NewResult =
%		case Result of
%			undefined -> opFor( S, P, O );
%			_ -> Result
%		end,
%	Receiver ! {QID, {Structure, NewResult}},
%	io:format("~p: exitting getNext~n", [self()]),
%	{ok, {Pattern, {Structure, NewResult}}}.


	
% estimate( #operator.specific#triple_pattern_spec.pattern, Stats ) ->
% 	Op = opFor( TriplePattern ),
% 	Estimate = heuristics:estimate( opFor( TriplePattern ), Stats ),
% %	io:format("e: ~p ~p~n", [Estimate, TriplePattern]),
% 	Op.%#operator{ estimatedSize = Estimate }.
	
% given a triple_pattern_spec, it will evaluate it and return bindings for contained variables.
evaluate( #operator{ type = ?TRIPLE_PATTERN_OP, varOrder = VarOrder, specific = #triple_pattern_spec{ evaluationFun = Fun} } ) ->
	% TODO should add real size to enable evaluation-time optimalisations
	#bindings{ varOrder = VarOrder, tuples = Fun() }.
%	Op#operator{ tuples = Fun() }.
	
%expand( #operator{ type = ?TRIPLE_PATTERN_OP, tuples = Tuples, specific = #triple_pattern_spec{ condensedIdx = Idx }} ) when is_list(Tuples) ->
%	lists:foldr( fun(CondensedTuple, Acc) -> [] ++ Acc end)

%% patterns with interpreted property containing two variables
testOpFor( {Var, ?rdf?type, Var2} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun(ID) ->
				Classes = lists:map(fun list_to_atom/1, model:classes()),
				sendToAll( Classes, members, [], ID ) end
		}
	}.
	
test() ->
	QID = make_ref(),
	Op = testOpFor( {1, ?rdf?type, 2} ),
	F = Op#operator.specific#triple_pattern_spec.evaluationFun,
	F({self(),QID}).
	%foldSetResponses(QID).

% ?a rdf:type ?a
opFor( {Var, ?rdf?type, Var} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:filter( fun(C) -> model:containsMember( C, C ) end, model:classes () ) end
		}
	};
	

	
% ?a rdf:type ?b
opFor( {Var1, ?rdf?type, Var2} = Pattern ) when ?IS_VAR(Var1) and ?IS_VAR(Var2) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var2, Var1],
%		condensedIdx = 1,
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:foldr( fun(C, Acc) -> model:membersOfAsPairs(C) ++ Acc end, [], model:classes() ) end
		}
	};


% ?a rdfs:subClassOf ?a
opFor( {Var, ?rdfs?subClassOf, Var} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				model:classes() end % every class is its subclass by definition
		}
	};


% ?a rdfs:subClassOf ?b
opFor( {Var1, ?rdfs?subClassOf, Var2} = Pattern ) when ?IS_VAR(Var1) and ?IS_VAR(Var2) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var2, Var1],
%		condensedIdx = 1,
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:foldr( fun(C, Acc) -> model:subClassesOfAsPairs(C) ++ Acc end, [], model:classes() ) end
		}
	};

	
% ?a rdfs:subPropertyOf ?a
opFor( {Var, ?rdfs?subPropertyOf, Var} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				model:properties() end % every property is its subproperty by definition.
		}
	};
	

% ?a rdfs:subPropertyOf ?b
opFor( {Var1, ?rdfs?subPropertyOf, Var2} = Pattern ) when ?IS_VAR(Var1) and ?IS_VAR(Var2) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var2, Var1],
%		condensedIdx = 1,
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:foldr( fun(P, Acc) -> model:subPropertiesOfAsPairs(P) ++ Acc end, [], model:properties() ) end
		}
	};


% ?a rdfs:domain ?a
opFor( {Var, ?rdfs?domain, Var} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:filter( fun(P) -> model:isDomainOf( P, P ) end, model:properties() ) end
		}
	};


% ?a rdfs:domain ?b
opFor( {Var1, ?rdfs?domain, Var2} = Pattern ) when ?IS_VAR(Var1) and ?IS_VAR(Var2)->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var1, Var2],
%		condensedIdx = 2,
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:foldr( fun(P, Acc) -> model:domainsOfAsPairs(P) ++ Acc end, [], model:properties() ) end
		}
	};


% ?a rdfs:range ?a
opFor( {Var, ?rdfs?range, Var} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:filter( fun(P) -> model:isRangeOf( P, P ) end, model:properties() ) end
		}
	};


% ?a rdfs:range ?b
opFor( {Var1, ?rdfs?range, Var2} = Pattern ) when ?IS_VAR(Var1) and ?IS_VAR(Var2)->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var1, Var2],
%		condensedIdx = 2,
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:foldr( fun(P, Acc) -> model:rangesOfAsPairs(P) ++ Acc end, [], model:properties() ) end
		}
	};

	

%% patterns with interpreted property containing one variable

% ?a rdf:type uri
opFor( {Var, ?rdf?type, Class} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				model:membersOf( Class ) end
		}
	};

	
% ?a rdfs:subClassOf uri
opFor( {Var, ?rdfs?subClassOf, Class} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				model:subClassesOf( Class ) end
		}
	};

% ?a rdfs:subPropertyOf uri
opFor( {Var, ?rdfs?subPropertyOf, Property} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				model:subPropertiesOf( Property ) end
		}
	};


% ?a rdfs:domain uri
opFor( {Var, ?rdfs?domain, Class} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:filter( fun(P) -> model:isDomainOf( Class, P ) end,  model:properties() ) end
		}
	};


% ?a rdfs:range uri
opFor( {Var, ?rdfs?range, Class} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:filter( fun( P ) -> model:isRangeOf( Class, P ) end, model:properties() ) end
		}
	};


% uri rdf:type ?a
opFor( {Url, ?rdf?type, Var} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:filter( fun( C ) -> model:containsMember( C, Url ) end, model:classes() ) end
		}
	};


% uri rdfs:subClassOf ?a
opFor( {Class, ?rdfs?subClassOf, Var} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:filter( fun( C ) -> model:isSubClassOf( Class, C ) end, model:classes() ) end
		}
	};


% uri rdfs:subPropertyOf ?a
opFor( {Property, ?rdfs?subPropertyOf, Var} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:filter( fun( P ) -> model:isSubPropertyOf( Property, P ) end, model:properties() ) end
		}
	};


% uri rdfs:domain ?a
opFor( {Property, ?rdfs?domain, Var} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				model:domainsOf( Property ) end
		}
	};


% uri rdfs:range ?a
opFor( {Property, ?rdfs?range, Var} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				model:rangesOf( Property ) end
		}
	};



%% patterns without interpreted variable containing two variables

% ?a ?b uri
opFor( {Var1, Var2, Url} = Pattern ) when ?IS_VAR(Var1) and ?IS_VAR(Var2)->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var2, Var1],
%		condensedIdx = 1,
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
%				Results =
					lists:foldr( fun(P, Acc) -> model:subjectsForAsPairs( P, Url ) ++ Acc end, [], model:properties() ) ++
					lists:foldr( 
						fun(P, Acc) -> 
							Op = opFor( {Var1, P, Url} ),
							Fun = Op#operator.specific#triple_pattern_spec.evaluationFun,
							lists:map( fun(S) -> {P, S} end, Fun() ) ++ Acc end,
						[], 
						connexion:interpretedProperties() )
				%lists:filter( fun({_, L}) -> L =/= [] end, Results)
			end
		}
	};
	


% uri ?a ?b
opFor( {Url, Var1, Var2} = Pattern ) when ?IS_VAR(Var1) and ?IS_VAR(Var2) ->
	#operator{
			type = ?TRIPLE_PATTERN_OP,
			varOrder = [Var1, Var2],
%			condensedIdx = 2,
			specific = #triple_pattern_spec{
			 	pattern = Pattern,
				evaluationFun = fun() ->
					Results =
						lists:foldr( fun(P, Acc) -> model:objectsForAsPairs( P, Url ) ++ Acc end, [], model:properties() ) ++
						lists:foldr( 
							fun(P, Acc) ->
								Op = opFor( {Url, P, Var2} ),
								Fun = Op#operator.specific#triple_pattern_spec.evaluationFun,
								lists:map( fun(O) -> {P, O} end, Fun() ) ++ Acc end, 
							[],
							connexion:interpretedProperties() ),
					lists:filter( fun({_, L}) -> L =/= [] end, Results )
				end
			}
		};

	
% ?a uri ?b	
opFor( {Var1, Url, Var2} = Pattern ) when ?IS_VAR(Var1) and ?IS_VAR(Var2) ->
	#operator{
			type = ?TRIPLE_PATTERN_OP,
			varOrder = [Var1, Var2],
			specific = #triple_pattern_spec{
			 	pattern = Pattern,
				evaluationFun = fun() ->
					model:pairsFor( Url ) end
			}
		};



%% patterns without interpreted property containing one variable "

% ?a uri1 uri2
opFor( {Var, Predicate, Object} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				model:subjectsFor( Predicate, Object ) end
		}
	};


% uri1 uri2 ?a
opFor( {Subject, Predicate, Var} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				model:objectsFor( Predicate, Subject ) end
		}
	};


% uri1 ?a uri2
opFor( {Subject, Var, Object} = Pattern ) when ?IS_VAR(Var) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				lists:filter( fun(P) -> model:containsPair( P, {Subject, Object} ) end, model:properties() ) end
		}
	};


% ?a ?b ?c
opFor( {Var1, Var2, Var3} = Pattern ) when ?IS_VAR(Var1) and ?IS_VAR(Var2) and ?IS_VAR(Var3) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [Var1, Var2, Var3],
%		condensedIdx = 2,
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				[] end
		}
	};
	
	
opFor( {_Subject, _Predicate, _Object} = Pattern ) ->
	#operator{
		type = ?TRIPLE_PATTERN_OP,
		varOrder = [],
		specific = #triple_pattern_spec{
		 	pattern = Pattern,
			evaluationFun = fun() ->
				[] end
		}
	}.
	
