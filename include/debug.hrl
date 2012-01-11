% special macro for actor, especially to enable/disable received message printing

-ifdef (debug_actor).
	-define (ACTOR_DEBUG(String, Args), io:format( String, Args )).
-else.
	-define (ACTOR_DEBUG(String, Args), true).
-endif.


% for other cases
-ifdef (debug).
	 -define (DEBUG(String, Args), io:format( String, Args )).
-else.
	-define (DEBUG(String, Args), true).
-endif.