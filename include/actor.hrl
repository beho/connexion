%% @author Svatopluk Sperka (isperka@fit.vutbr.cz)
%% {@date}
%% @headerfile

-export ([
%	behaviour_info/1,
	new/1, new/2
]).
-compile(export_all).

-include ("messaging.hrl").
-include ("debug.hrl").

%behaviour_info( callbacks ) ->
%	[{initialize/2,
%	  methodFor/1,
%	  stop/1}];
%behaviour_info( _ ) ->
%	undefined.

%% @doc Functionality for turning erlang processes into more actor getstalt,
%% i.e. they can be thought of as active objects communicating by means of 
%% message passing while mantaining less verbose syntax than OTP.

-record (actor_state, {name, userState}).

%% @doc Macro for defining public methods of an actor. Three parameters to 
%% be given are Args, {From, QID} and UserState
-define( M( Selector ), Selector -> fun Selector/3 ).

%% @doc This macro defines default handler for unknown selector (atom denoting
%% method to be called).
-define( DNU_HANDLER, _ -> {error, {do_not_understand, Selector}} ).

%% @doc Default name for unnamed actors. When this name is set when creating 
%% actor, it won't be globally registered and thus will only be accesible using its PID.
-define( UNNAMED, unnamed ).


%% @doc Constructor of an actor. When atom is supplied it gets registered under that name.
%% @spec new( atom() ) -> atom()
new( Name ) when is_atom( Name ) ->
	proc_lib:start( ?MODULE, basicInitialize, [Name, [], self()]);

%% @spec new( list() ) -> pid()
new( Args ) when is_list( Args ) ->
	proc_lib:start( ?MODULE, basicInitialize, [?UNNAMED, Args, self()] ).

%% @spec new( atom(), list() ) -> atom()
new( Name, Args ) ->
	proc_lib:start( ?MODULE, basicInitialize, [Name, Args, self()]).	
	

%% @doc Implements fundamental initialisation of actor's state including
%% custom user's state by calling initialize. 
basicInitialize( Name, Args, Parent ) ->
	?ACTOR_DEBUG("~s : basicInitialize~n", [nameToPrint( Name )]),
	ActorState = #actor_state{ name = Name },
	Id = 
		if 
			Name =/= ?UNNAMED ->
				register( Name, self() ),
				Name;
			true -> self()
		end,
	UserState = initialize( Name, Args ),
	case UserState of
		{error, Reason} -> exit( Reason );
		stop -> self() ! stop;
		_ ->
			InitializedState = ActorState#actor_state{ userState = UserState },
			proc_lib:init_ack( Parent, Id ),
			handleMessage( InitializedState )
	end.

%% @doc Handles incoming messages. Checks if they are in correct format and
% if there is a method for message's selector.
%% @TODO handling custom messages as EXIT ?
handleMessage( State = #actor_state{ userState = UserState, name = Name } ) ->
	Action = receiveMessageAndConstructMethodCall( UserState ),
	
	case Action of
		{error, bad_message_format, Msg} ->
			io:format("~s: error > bad_message_format (~p)~n", [nameToPrint( Name ), Msg]),
			% log invalid message format
			handleMessage( UserState );
			
		{_, {error, {do_not_understand, Selector}}, _} ->
			io:format("error: ~w : do_not_understand ~w~n", [Name, Selector]),
			% log unknown message
			handleMessage( UserState );
		
		{Selector, Method, Params} ->
			?ACTOR_DEBUG("~s : ~w~n", [nameToPrint( Name ), Selector]),
			
			try
				case apply( Method, Params ) of
					{ok, NewUserState} ->
						handleMessage( State#actor_state{ userState = NewUserState } );
					
					stop ->
						stop( UserState ),
						exit( normal );
					
					_ -> 
						io:format( "~s > ~p: Unknown method return term.~n", [nameToPrint( Name ), Action] ),
						%handleMessage( State )
						exit( error )
				end
			catch
				error:Reason -> io:format("~p~n", [{Reason, erlang:get_stacktrace()}]) 
			end
	end.
		
%% @doc Constructs a triple representing method call.
receiveMessageAndConstructMethodCall( UserState ) ->
	receive
		{type, Args, {_Receiver, _QID} = ID} ->
			{type, fun type/3, [Args, ID, UserState]};
		
		{Selector, Args, {_Receiver, _QID} = ID} ->
			{Selector, methodFor( Selector ), [Args, ID, UserState]};
%			constructMethodCall( Selector, [Args, ID, UserState]);
			
		stop -> stop;
		
		type -> type;
			
		Msg -> {error, bad_message_format, Msg}
	end.
	
type( _, {Receiver, QID}, UserState) ->
	Receiver ! {QID, ?MODULE},
	{ok, UserState}.

%constructMethodCall( Selector, Params ) ->
%	{Selector, methodFor( Selector ), Params}.
	
%% @doc Returns actor's name or its PID as a string depending if it is named
%% actor or not. For debugging purposes.
nameToPrint( Name ) ->
	if
		Name =/= ?UNNAMED -> Name;
		true -> pid_to_list( self() ) ++ " [" ++ ?MODULE_STRING ++ "]"
	end.
