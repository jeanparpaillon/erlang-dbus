libsubdir = $(ERLANG_INSTALL_LIB_DIR)/$(OTP_APP)-$($(OTP_APP)_VSN)
bindir = $(libsubdir)/bin
ebindir = $(libsubdir)/ebin
incdir = $(libsubdir)/include

inc_HEADERS = $($(OTP_APP)_HDRS)
beam_FILES = $($(OTP_APP)_SRCS:.erl=.beam)

ebin_DATA = $(beam_FILES) $(OTP_APP:=.app)
EXTRA_DIST = $($(OTP_APP)_SRCS) $(OTP_APP:=.app-in)
CLEANFILES = $(beam_FILES) $(OTP_APP:=.app)

include $(top_srcdir)/rules/rules.mk
