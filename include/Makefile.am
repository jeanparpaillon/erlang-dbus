abs_top_srcdir = @abs_top_srcdir@
abs_top_builddir = @abs_top_builddir@

SUBST = sed								\
	-e 's,[@]abs_top_srcdir[@],$(abs_top_srcdir),g'			\
	-e 's,[@]abs_top_builddir[@],$(abs_top_builddir),g'		\
	-e 's,[@]ERLANG_LIB_DIR_xmerl[@],$(ERLANG_LIB_DIR_xmerl),g'

%: %.in
	$(SUBST) $< > $@

noinst_DATA = config.hrl

EXTRA_DIST = config.hrl.in
CLEANFILES = config.hrl

SUFFIXES = .in
