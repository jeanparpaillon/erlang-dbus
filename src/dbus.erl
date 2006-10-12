-module(dbus).

-import(dberl.proxy).
-import(dberl.marshaller).
-import(dberl.bus).

-include("dbus.hrl").

%% api
-export([start_link/0, stop/0]).

-export([make/0, test/0, get_object/3, call/2, call/3, wait_ready/1]).

-define(SERVER, ?MODULE).
-define(PORT, 1236).
-define(HOST, "localhost").

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

test() ->
    {ok, Bus} = bus:start_link(),
    ok = dbus:wait_ready(Bus),
    io:format("Ready~n"),
    {ok, BusObj} = dbus:get_object(Bus, 'org.freedesktop.DBus', '/'),
    {ok, BusIface} = proxy:interface(BusObj, 'org.freedesktop.DBus'),
    {ok, Header1, Reply1} = proxy:call(BusIface, 'RequestName', ["org.za.hem.DBus", 0]),
    io:format("ListNames: ~p ~p~n", [Header1, Reply1]),

    {ok, Remote_object} = dbus:get_object(Bus, 'org.designfu.SampleService', '/SomeObject'),
    {ok, Iface} = proxy:interface(Remote_object, 'org.designfu.SampleInterface'),
%%     Var = #variant{type=string, value="Hello from Erlang!"},
%%      Var = #variant{type={array, string}, value=["Hello", "from", "Erlang!"]},
%%     Var = <<"Hello from Erlang">>,
%%     Var = #variant{type={struct, [int16, string]}, value=[17, "Hello from Erlang!"]},
    Var = #variant{type={struct, [int16, string]}, value={17, "Hello from Erlang!"}},
    {ok, Header, ReplyList} = proxy:call(Iface, 'HelloWorld', [Var]).

get_object(Bus, Service, Path) ->
    proxy:start_link(Bus, Service, Path).

call(Bus, Header) ->
    gen_server:cast(Bus, {call, Header, self()}).

call(Bus, Header, From) ->
    gen_server:cast(Bus, {call, Header, From, self()}).

wait_ready(Bus) ->
    io:format("wait_ready enter ~p~n", [Bus]),
    ok = gen_server:call(Bus, wait_ready),
    io:format("wait_ready exit ~p~n", [Bus]),
    ok.

make() ->
    Modules = [
	       "call",
	       "dbus"
	      ],

    Prefix = "/home/mikael/svn/dberl/src/",
    make_modules(Prefix, Modules),

    Modules2 = [
	       "auth",
	       "bus",
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
