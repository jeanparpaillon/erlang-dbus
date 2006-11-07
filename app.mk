libsubdir = $(ERLANG_INSTALL_LIB_DIR)/$(OPT_APP)-$($(OPT_APP)_VSN)
bindir = $(libsubdir)/bin
ebindir = $(libsubdir)/ebin
incdir = $(libsubdir)/include

inc_HEADERS = $($(OPT_APP)_HDRS)
beam_FILES = $($(OPT_APP)_SRCS:.erl=.beam)

ebin_DATA = $(beam_FILES) $(OPT_APP:=.app) $(OPT_APP:=.boot)
EXTRA_DIST = $($(OPT_APP)_SRCS) $(OPT_APP:=.app-in) $(OPT_APP:=.rel.in)
CLEANFILES = $(beam_FILES) $(OPT_APP:=.app) $(OPT_APP:=.boot) $(OPT_APP:=.rel)	\
$(OPT_APP:=.script)

include $(top_srcdir)/rules.mk
