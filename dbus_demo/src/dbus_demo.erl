%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc
%%

-module(dbus_demo).

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
	 start_link/0
	]).

-define(SERVER, ?MODULE).
-define(HOST, "localhost").
-define(PORT, 1236).

-record(state, {
	  bus,
	  bus_obj
	 }).

-export([test/0, run_test/1]).

test() ->
    Pid = case start_link() of
	      {ok, Pid1} -> Pid1;
	      {error, {already_started, Pid1}} -> Pid1
	  end,
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
     {ok, Bus} = dbus_bus_reg:get_bus(#bus_id{scheme=tcp,
					 options=[{host, Host},
						  {port, Port}]}),
%%     {ok, Bus} = bus_reg:get_bus(Host, Port),
    true = link(Bus),
    ok = dbus_bus:wait_ready(Bus),
    io:format("Ready~n"),

    {ok, Service} = dbus_bus:get_service(Bus, 'org.freedesktop.DBus'),

    {ok, BusObj} = dbus_remote_service:get_object(Service, '/'),
    true = link(BusObj),
    io:format("BusObj: ~p~n", [BusObj]),

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

%%     {ok, Rb} = dbus_bus:get_service(Bus, 'org.gnome.Rhythmbox'),

%% %%     {ok, RbShellObj} = dbus_bus:get_object(Bus, 'org.gnome.Rhythmbox', '/org/gnome/Rhythmbox/Shell'),
%%     {ok, RbShellObj} = dbus_remote_service:get_object(Rb, '/org/gnome/Rhythmbox/Shell'),
%%     {ok, RbShell} = dbus_proxy:interface(RbShellObj, 'org.gnome.Rhythmbox.Shell'),
%%     {ok, RbPlayerObj} = dbus_bus:get_object(Bus, 'org.gnome.Rhythmbox', '/org/gnome/Rhythmbox/Player'),
%%     {ok, RbPlayer} = dbus_proxy:interface(RbPlayerObj, 'org.gnome.Rhythmbox.Player'),
%% %%    ok = dbus_proxy:connect_signal(RbPlayer, 'elapsedChanged', mytag),
%% %%    ok = dbus_proxy:connect_signal(RbPlayer, 'playingUriChanged', mytag),

%%     {ok, Uri} = dbus_proxy:call(RbPlayer, 'getPlayingUri'),
%%     {ok, Song} = dbus_proxy:call(RbShell, 'getSongProperties', [Uri]),
%%     io:format("Song: ~p~n~p~n", [Uri, Song]),

%% %%     io:format("manager: ~p~n", [dbus_proxy:call(RbShell, 'getPlaylistManager')]),
%% %%     io:format("player: ~p~n", [dbus_proxy:call(RbShell, 'getPlayer')]),

    {ok, Service} = dbus_bus:get_service(Bus, 'org.designfu.SampleService'),

%%     {ok, Remote_object} = dbus_bus:get_object(Bus, 'org.designfu.SampleService', '/SomeObject'),
    {ok, Remote_object} = dbus_remote_service:get_object(Service, '/SomeObject'),
    io:format("Remote_object: ~p~n", [Remote_object]),
    {ok, Iface} = dbus_proxy:interface(Remote_object, 'org.designfu.SampleInterface'),
    ok = dbus_proxy:connect_signal(Iface, 'OnClick', mytag),
%%     Var = #variant{type=string, value="Hello from Erlang!"},
%%      Var = #variant{type={array, string}, value=["Hello", "from", "Erlang!"]},
%%     Var = <<"Hello from Erlang">>,
%%     Var = #variant{type={struct, [int16, string]}, value=[17, "Hello from Erlang!"]},
    Var = #variant{type={struct, [int16, string]}, value={17, "Hello from Erlang!"}},
    {ok, Reply1} = dbus_proxy:call(Iface, 'HelloWorld', [Var]),
    io:format("HelloWorld 1: ~p~n", [Reply1]),

    Var1 = #variant{type={struct, [int16, string]}, value={17, "Hello from Erlang no 2!"}},
    {ok, Reply2} = dbus_proxy:call(Iface, 'HelloWorld', [Var1]),
    io:format("HelloWorld 2: ~p~n", [Reply2]),
%%     ok = dbus_proxy:stop(Remote_object),
%%     ok = dbus_bus:stop(Bus),

%%     {ok, PeerIface} = dbus_proxy:interface(BusObj, 'org.freedesktop.DBus.Peer'),
%%     dbus_proxy:call(PeerIface, 'Ping'),
    ok.
