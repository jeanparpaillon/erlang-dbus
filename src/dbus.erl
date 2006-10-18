-module(dbus).

-import(dberl.proxy).
-import(dberl.bus).
-import(dberl.bus_reg).

-behaviour(gen_server).

-include("dbus.hrl").

%% gen_server callbacks
-export([
	 init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2
	]).

%% api
-export([
	 start_link/0,
	 stop/0]).

-define(SERVER, ?MODULE).
-define(HOST, "localhost").
-define(PORT, 1236).

-record(state, {
	  bus,
	  bus_obj
	 }).

-export([make/0, test/0, get_object/3, call/2, call/3, wait_ready/1]).

stop() ->
    gen_server:cast(todo, stop).

get_object(Bus, Service, Path) ->
    bus:get_object(Bus, Service, Path).

call(Bus, Header) ->
    bus:call(Bus, Header).

call(Bus, Header, From) ->
    bus:call(Bus, Header, From).

wait_ready(Bus) ->
    bus:wait_ready(Bus).

make() ->
    Modules = [
	       "dberl",
	       "dbus",
	       "hello"
	      ],

    Prefix = "/home/mikael/svn/dberl/src/",
    make_modules(Prefix, Modules),

    Modules2 = [
	       "auth",
	       "bus",
	       "bus_reg",
	       "call",
 	       "connection",
	       "gen_dbus",
	       "introspect",
	       "marshaller",
	       "message",
	       "proxy",
	       "service",
	       "service_reg",
	       "sup",
	       "tcp_conn",
	       "transport"
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


test() ->
    {ok, Pid} = start_link(),
    run_test(Pid).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [?HOST, ?PORT], []).

run_test(Pid) ->
    gen_server:call(Pid, run_test).

%%
%% gen_server callbacks
%%
init([Host, Port]) ->
    self() ! {setup, Host, Port},
    {ok, #state{}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(run_test, _From, State) ->
    do_test(State),
    {reply, ok, State};

handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info({setup, Host, Port}, State) ->
%%     BusSpec = {{bus, Host, Port}, {dberl.bus,connect,[Host,Port]}, permanent, 10000, worker, [dberl.bus]},
    
%%     {ok, Bus} = supervisor:start_child(dberl.sup, BusSpec),
    {ok, Bus} = bus_reg:get_bus(Host, Port),
    true = link(Bus),
    ok = bus:wait_ready(Bus),
    io:format("Ready~n"),

%%     {ok, Service} = dberl.service_reg:export_service('org.za.hem.DBus'),
%%     {ok, Local_object} = hello:start_link(Service, '/Root'),

    {ok, BusObj} = bus:get_object(Bus, 'org.freedesktop.DBus', '/'),
    true = link(BusObj),
    io:format("BusObj: ~p~n", [BusObj]),

%%     BusObj = undefined,

    {noreply, State#state{bus=Bus,
			  bus_obj=BusObj
			 }};
handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.


do_test(State) ->
    Bus = State#state.bus,

%%     ok = proxy:stop(BusObj),
%%     io:format("RequestName: ~p~n", [Header1]),

    {ok, Service} = bus:export_service(Bus, 'org.za.hem.DBus'),
    {ok, Local_object} = hello:start_link(Service, '/Root'),

    {ok, Remote_object} = bus:get_object(Bus, 'org.designfu.SampleService', '/SomeObject'),
    io:format("Remote_object: ~p~n", [Remote_object]),
    {ok, Iface} = proxy:interface(Remote_object, 'org.designfu.SampleInterface'),
    ok = proxy:connect_signal(Iface, 'OnClick', mytag),
%%     Var = #variant{type=string, value="Hello from Erlang!"},
%%      Var = #variant{type={array, string}, value=["Hello", "from", "Erlang!"]},
%%     Var = <<"Hello from Erlang">>,
%%     Var = #variant{type={struct, [int16, string]}, value=[17, "Hello from Erlang!"]},
    Var = #variant{type={struct, [int16, string]}, value={17, "Hello from Erlang!"}},
    {ok, Reply1} = proxy:call(Iface, 'HelloWorld', [Var]),
    io:format("HelloWorld 1: ~p~n", [Reply1]),

    Var1 = #variant{type={struct, [int16, string]}, value={17, "Hello from Erlang no 2!"}},
    {ok, Reply2} = proxy:call(Iface, 'HelloWorld', [Var1]),
    io:format("HelloWorld 2: ~p~n", [Reply2]),
%%     ok = proxy:stop(Remote_object),
%%     ok = bus:stop(Bus),

%%     {ok, PeerIface} = proxy:interface(BusObj, 'org.freedesktop.DBus.Peer'),
%%     proxy:call(PeerIface, 'Ping'),
    ok.
