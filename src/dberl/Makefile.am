include $(top_srcdir)/rules.mk

OPT_APP = dberl
dberl_VSN = 0.1

libsubdir = $(ERLANG_INSTALL_LIB_DIR)/$(OPT_APP)-$($(OPT_APP)_VSN)
ebindir = $(libsubdir)/ebin/dberl

noinst_SRCS = \
auth.erl \
bus.erl \
bus_reg.erl \
call.erl \
connection.erl \
gen_dbus.erl \
introspect.erl \
marshaller.erl \
message.erl \
proxy.erl \
remote_service.erl \
service.erl \
service_reg.erl \
sup.erl \
tcp_conn.erl \
transport.erl

noinst_HDRS = dbus.hrl

AM_ERLCFLAGS = -I$(srcdir)/.. -I/usr/lib/erlang/lib/xmerl-1.0.5/include

beam_FILES = $(noinst_SRCS:.erl=.beam)
ebin_DATA = $(beam_FILES)

EXTRA_DIST = $(noinst_SRCS)
CLEANFILES = $(beam_FILES)
