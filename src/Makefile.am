include $(top_srcdir)/app.mk

SUBDIRS = dberl

OPT_APP = dberl

dberl_VSN = 0.1

dberl_SRCS = \
dberl.erl

dberl_HDRS = dbus.hrl

AM_ERLCFLAGS = -I$(srcdir) -I/usr/lib/erlang/lib/xmerl-1.0.5/include
AM_ERL_FLAGS = -pa /home/mikael/tmp/cvs/jungerl/lib/unixdom_drv/ebin

run:
	$(ERL) $(AM_ERL_FLAGS) -name eryate -boot dberl
#	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -name eryate -boot yate -sasl sasl_error_logger '{file,"sasl.log"}' -mnesia dir '"/var/tmp/mnesia/eryate"' -s appmon start
#	$(ERL) -pz /usr/lib/yxa/ebin -boot hello
#	$(ERL) -pz /usr/lib/yxa/ebin -boot dberl -s appmon start

#	$(ERL) -pz /usr/lib/yxa/ebin -run crypto start -s application start sasl -s appmon start -s application start dberl
