version = 0.1
PROJECT = erlang-dbus
PROJECT_VERSION = $(shell git describe --always --tags 2> /dev/null || echo $(version))

DEPS = inert procket
dep_procket = git https://github.com/msantos/procket.git master
dep_inert = git https://github.com/msantos/inert.git 0.2.1

include erlang.mk

fetch:: $(ALL_DEPS_DIRS)
	for d in $(ALL_DEPS_DIRS); do \
	  $(MAKE) -C $$d $@ || true; \
	done

.PHONY: fetch
