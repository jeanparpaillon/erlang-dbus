-module(dbus).

-import(dberl.proxy).
-import(dberl.bus).

-include("dbus.hrl").

%% api
-export([connect/2, stop/0]).

-export([make/0, test/0, get_object/3, call/2, call/3, wait_ready/1]).

-define(PORT, 1236).
-define(HOST, "localhost").

connect(Host, Port) when is_list(Host), is_integer(Port) ->
    bus:connect(Host, Port).

stop() ->
    gen_server:cast(todo, stop).

test() ->
    {ok, Bus} = dbus:connect(?HOST, ?PORT),
    ok = dbus:wait_ready(Bus),
    io:format("Ready~n"),
    {ok, BusObj} = dbus:get_object(Bus, 'org.freedesktop.DBus', '/'),

    {ok, BusIface} = proxy:interface(BusObj, 'org.freedesktop.DBus'),
    {ok, Header1} = proxy:call(BusIface, 'RequestName', ["org.za.hem.DBus", 0]),
    io:format("RequestName: ~p~n", [Header1]),

    {ok, Remote_object} = dbus:get_object(Bus, 'org.designfu.SampleService', '/SomeObject'),
    {ok, Iface} = proxy:interface(Remote_object, 'org.designfu.SampleInterface'),
%%     Var = #variant{type=string, value="Hello from Erlang!"},
%%      Var = #variant{type={array, string}, value=["Hello", "from", "Erlang!"]},
%%     Var = <<"Hello from Erlang">>,
%%     Var = #variant{type={struct, [int16, string]}, value=[17, "Hello from Erlang!"]},
    Var = #variant{type={struct, [int16, string]}, value={17, "Hello from Erlang!"}},
    {ok, Header} = proxy:call(Iface, 'HelloWorld', [Var]),

    Var1 = #variant{type={struct, [int16, string]}, value={17, "Hello from Erlang no 2!"}},
    proxy:call(Iface, 'HelloWorld', [Var1]),

%%     {ok, PeerIface} = proxy:interface(BusObj, 'org.freedesktop.DBus.Peer'),
%%     proxy:call(PeerIface, 'Ping'),
    ok.


get_object(Bus, Service, Path) ->
    proxy:start_link(Bus, Service, Path).

call(Bus, Header) ->
    bus:call(Bus, Header).

call(Bus, Header, From) ->
    bus:call(Bus, Header, From).

wait_ready(Bus) ->
    bus:wait_ready(Bus).

make() ->
    Modules = [
	       "dbus"
	      ],

    Prefix = "/home/mikael/svn/dberl/src/",
    make_modules(Prefix, Modules),

    Modules2 = [
	       "auth",
	       "bus",
	       "call",
	       "connection",
	       "introspect",
	       "marshaller",
	       "message",
	       "proxy",
	       "tcp_conn"		
	       ],
    Prefix2 = "/home/mikael/svn/dberl/src/dberl/",
    make_modules(Prefix2, Modules2).


make_modules(Prefix, Modules) ->
    Files = lists:map(fun(File) -> Prefix ++ File end, Modules),

    make:files(Files,
	       [
		load,
		{i, "/usr/lib/erlang/lib/xmerl-1.0.5/include"},
		{outdir, Prefix}
	       ]).
