ERLC = erlc

erl_FILES = \
dberl.erl \
dbus.erl \
hello.erl \
hello_app.erl \
hello_sup.erl

beam_FILES = $(erl_FILES:.erl=.beam)

ERLFLAGS =
EFLAGS = $(ERLFLAGS)

%.beam: %.erl
	$(ERLC) $(EFLAGS) -I $(top_srcdir)/include $<

%.app: %.app-in
	cp $< $@

%.boot: %.rel %.app
	$(ERLC) $(EFLAGS) $<

all: hello.boot $(beam_FILES)

run:
	erl -pz /usr/lib/yxa/ebin -boot hello

#	erl -pz /usr/lib/yxa/ebin -run crypto start -s application start sasl -s appmon start -s application start dberl

