PROJECT = dbus
PROJECT_VERSION = 0.8.0

BUILD_DEPS = hexer_mk

dep_hexer_mk = git https://github.com/inaka/hexer.mk.git 1.1.0

DEP_PLUGINS = hexer_mk

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)
