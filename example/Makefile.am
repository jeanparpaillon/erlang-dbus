include $(top_srcdir)/rules.mk

OPT_APP = hello

hello_VSN = 0.1

hello_SRCS = \
hello.erl \
hello_app.erl \
hello_sup.erl

AM_ERLCFLAGS = -I$(top_srcdir)/src
AM_ERL_FLAGS = -pz $(top_builddir)/src

run:
	$(ERL) $(AM_ERL_FLAGS) -boot hello
