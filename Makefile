PROJECT = dbus
PROJECT_VERSION = 0.8.0

BUILD_DEPS = hexer_mk

dep_hexer_mk = git https://github.com/inaka/hexer.mk.git 1.1.0

DEP_PLUGINS = hexer_mk

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)

## For building and cleaning the cpp based example service automatically

test-build:: test/dbus_client_SUITE_data/example-service/example-service

clean::
	@[ ! -f test/dbus_client_SUITE_data/example-service/Makefile ] || $(MAKE) -C test/dbus_client_SUITE_data/example-service distclean

test/dbus_client_SUITE_data/example-service/Makefile: test/dbus_client_SUITE_data/example-service/main.cpp test/dbus_client_SUITE_data/example-service/example-service.pro
	cd test/dbus_client_SUITE_data/example-service && qmake

test/dbus_client_SUITE_data/example-service/example-service: test/dbus_client_SUITE_data/example-service/Makefile
	$(MAKE) -C test/dbus_client_SUITE_data/example-service
