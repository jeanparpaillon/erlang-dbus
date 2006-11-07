include $(top_srcdir)/app.mk

OPT_APP = hello

hello_VSN = 0.1

hello_SRCS = \
dbus.erl \
hello.erl \
hello_app.erl \
hello_sup.erl

AM_ERLCFLAGS = -I$(top_srcdir)/src
AM_ERL_FLAGS = -pz $(top_builddir)/src -pz /usr/lib/yxa/ebin

run:
	$(ERL) $(AM_ERL_FLAGS) -boot hello
