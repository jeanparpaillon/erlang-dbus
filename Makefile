all:
	rebar3 compile

deps:
	rebar3 get-deps

tests:
	rebar3 eunit --cover

docs:
	rebar3 edoc

dialyze:
	rebar3 dialyzer

.PHONY: all deps docs tests dialyze