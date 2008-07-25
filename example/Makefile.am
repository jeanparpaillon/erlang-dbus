include $(top_srcdir)/rules/app.mk

OTP_APP = hello

hello_VSN = 0.1

hello_SRCS = \
dbus.erl \
hello.erl \
hello_app.erl \
hello_sup.erl

AM_ERLCFLAGS = -I$(top_builddir)/dberl/include -I$(top_srcdir)/dberl/src
AM_ERL_FLAGS = -pz $(top_builddir)/dberl/src


run:
	$(ERL) $(AM_ERL_FLAGS) $(ERL_FLAGS) -boot hello
