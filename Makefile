version = 0.1
PROJECT = dbus
PROJECT_VERSION = $(shell git describe --always --tags 2> /dev/null || echo $(version))

DEPS = edown
#DEPS += annotations
#DEP_PLUGINS = annotations

dep_annotations_commit = 9f8a800

EDOC_OPTS = {app_default, "http://www.erlang.org/doc/"} \
           ,{doclet, edown_doclet} \
           ,{top_level_readme, {"$(CURDIR)/doc/README.md", "http://github.com/lizenn/erlang-dbus"}}

#ANNOTATIONS = logging

include erlang.mk

fetch:: $(ALL_DEPS_DIRS)
	for d in $(ALL_DEPS_DIRS); do \
	  $(MAKE) -C $$d $@ || true; \
	done

.PHONY: fetch
