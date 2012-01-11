APPLICATION := ek
SOURCES := $(wildcard src/*.erl)
HEADERS := $(wildcard src/*.hrl)
MODULES := $(patsubst src/%.erl,%,$(SOURCES))
BEAMS := $(patsubst %,ebin/%.beam,$(MODULES))

ERL_LIB=/opt/local/lib/erlang/lib/erl_interface-3.7.2
CFLAGS=-g -Wall -std=c99 -I/usr/local/include -I$(ERL_LIB)/include
LDFLAGS=-L. -L$(ERL_LIB)/lib -L/usr/local/lib -lraptor

comma := ,
e :=
space := $(e) $(e)
MODULELIST := $(subst $(space),$(comma),$(MODULES))

TEST_SOURCES := $(wildcard test/*.erl)
TEST_BEAMS := $(patsubst %.erl,%.beam, $(TEST_SOURCES))

.PHONY: all clean dialyze

#all: $(APPLICATION)
all: compile raptor_parser

compile:
	@erl -make
	
run: all
	erl -pa ebin/ -s connexion setup

test: all $(TEST_BEAMS) util/run_tests.beam
	@echo Running tests
	erl -pa util/ -pa ebin/ -pa test/ -noinput -s run_tests run
	
test_shell: all $(TEST_BEAMS)
	@erl -pa ebin/ -pa test/
	
test/%.beam: test/%.erl
	@echo Compiling test $<
	@erlc +debug_info -o test/ $<

raptor_parser: ebin/raptor_parser.o
	@echo Compiling port for raptor parser
	@gcc $(LDFLAGS) $< -lerl_interface -lei -lpthread -o ebin/$@

ebin/%.o: src/parsers/%.c
	@gcc -g $(CFLAGS) -o $@ -c $<

util/%.beam: util/%.erl
	@erlc -o util/ util/run_tests.erl
	
#ebin:
#	@mkdir ebin/
	
doc: doc/edoc-info

dialyzer:
	@echo Running dialyzer on sources
	@dialyzer --src -r src/
	
util/%.beam: util/%.erl
	@erlc -o util/ util/run_tests.erl
	
clean:
	@echo Cleaning
	@rm -f ebin/*.beam experiments/*.beam ebin/raptor_parser ebin/*.o test/*.beam doc/*.{html,css,png} doc/edoc-info