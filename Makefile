all:
	rebar3 compile

deps:
	rebar3 get-deps

tests:
	rebar3 eunit --cover

dialyze:
	rebar3 dialyzer

.PHONY: all deps tests dialyze