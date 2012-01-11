-module (run_tests).

-export ([run/0]).

-include_lib( "eunit/include/eunit.hrl" ).

run() ->
	model:initialize(),
	eunit:test( plan_heap_tests, [verbose] ),
	eunit:test( class_tests, [verbose] ),
	eunit:test( property_tests, [verbose] ),
	eunit:test( model_tests, [verbose] ),
	eunit:test( subclass_tests, [verbose] ),
	eunit:test( subproperty_tests, [verbose] ),
	eunit:test( domainrange_tests, [verbose] ),
	halt().
	