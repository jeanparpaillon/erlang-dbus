all: build

build:
	rebar3 compile

deps:
	rebar3 get-deps

checks: fmt lint dialyze

fmt:
	rebar3 fmt --check

lint:
	rebar3 lint

dialyze:
	rebar3 dialyzer

tests:
	rebar3 eunit --cover

docs:
	rebar3 edoc

clean:
	rebar3 clean

.PHONY: all clean
.PHONY: deps build checks tests docs
.PHONY: fmt lint dialyze checks
