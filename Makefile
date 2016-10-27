PROJECT = dbus
PROJECT_VERSION = 0.7

BUILD_DEPS = hexer_mk

dep_hexer_mk = git https://github.com/inaka/hexer.mk.git 1.1.0

DEP_PLUGINS = hexer_mk

include erlang.mk
