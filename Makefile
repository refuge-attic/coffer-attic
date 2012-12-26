
all: deps compile
	@echo "done!"

deps:
	@rebar get-deps

compile:
	@rebar compile

clean:
	@rebar clean

test: all
	@rebar eunit skip_deps=true
