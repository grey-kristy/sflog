
REBAR = rebar

all: compile

compile:
	$(REBAR) compile

clean:
	- rm $(CURDIR)/erl_crash.dump
	$(REBAR) skip_deps=true clean	

