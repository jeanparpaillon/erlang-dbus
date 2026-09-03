PROPS_ITER ?= 1000

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

tests: eunit props ct

eunit:
	rebar3 eunit --cover

props:
	rebar3 proper --cover -n $(PROPS_ITER)

# The Unix fd suite needs a session bus and a `dbus-daemon' on PATH; it skips
# rather than fails without them.
ct:
	rebar3 ct

coverage:
	rebar3 cover --verbose
	xdg-open _build/test/cover/index.html

docs:
	rebar3 edoc

clean:
	rebar3 clean

.PHONY: all clean
.PHONY: deps build checks tests props docs
.PHONY: fmt lint dialyze checks eunit ct
