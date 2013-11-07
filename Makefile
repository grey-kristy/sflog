
REBAR = rebar

all: compile

compile:
	$(REBAR) compile
	erlc -o test/ test/*.erl

clean:
	-rm $(CURDIR)/erl_crash.dump
	-rm $(CURDIR)/test/*.beam
	$(REBAR) skip_deps=true clean	

test: compile
	erl -pa ebin/ test/ -s sflog_test test_all -s init stop


.PHONY:	clean test