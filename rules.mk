SUBST = sed \
	-e 's,[@]ERLANG_LIB_VER_kernel[@],$(ERLANG_LIB_VER_kernel),g' \
	-e 's,[@]ERLANG_LIB_VER_stdlib[@],$(ERLANG_LIB_VER_stdlib),g' \
	-e 's,[@]ERLANG_LIB_VER_sasl[@],$(ERLANG_LIB_VER_sasl),g' \
	-e 's,[@]ERLANG_LIB_VER_crypto[@],$(ERLANG_LIB_VER_crypto),g' \
	-e 's,[@]VSN[@],$($(OPT_APP)_VSN),g'

%.rel: %.rel.in
	$(SUBST) $< > $@

%.beam: %.erl
	$(ERLC) $(AM_ERL_FLAGS) $(ERL_FLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS) $<

%.app: %.app-in
	cp $< $@

%.boot: %.rel %.app
	$(ERLC) $(AM_ERL_FLAGS) $(ERL_FLAGS) $(AM_ERLCFLAGS) $(ERLCFLAGS) $<
