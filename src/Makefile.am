include $(top_srcdir)/rules/app.mk

SUBDIRS = dberl

OPT_APP = dberl

dberl_VSN = 0.1

dberl_SRCS = \
dberl.erl

dberl_HDRS = dbus.hrl

AM_ERLCFLAGS = -I$(top_builddir)/include -I$(srcdir)	\
-I$(ERLANG_LIB_DIR_xmerl)/include
AM_ERL_FLAGS =

run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name eryate -boot dberl
