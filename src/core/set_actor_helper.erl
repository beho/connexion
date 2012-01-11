-module (set_actor_helper).

-export ([subSetOf/5, superSetOf/2]).


subSetOf( Self, Set, SuperSets, SuperSetOfSelector, QID ) ->
	%io:format("~w~n~w~n", [Set, SuperSets]),
	case sets:is_element( Set, SuperSets ) of
		true -> SuperSets;
		false ->
			Set ! {SuperSetOfSelector, Self, {self(), QID}},
			NewSuperSets = sets:add_element( Set, SuperSets ),
			receive
				{QID, ok} -> NewSuperSets
			end
	end.
	
superSetOf( Set, SubSets ) ->
	%io:format("~w~n~w~n", [Set, SubSets]),
	sets:add_element( Set, SubSets ).
	

