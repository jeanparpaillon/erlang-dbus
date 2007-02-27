%%%
%%% @doc       D-BUS application module
%%% @author    Mikael Magnusson <mikma@users.sourceforge.net>
%%% @copyright 2006 Mikael Magnusson
%%%
-module(dberl).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% api:s
-export([start/0, make/0]).

%% application callbacks
start(normal, []) ->
    error_logger:logfile({open, "dberl.log"}),
    dberl.sup:start_link().

stop(_State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec start() -> Result
%%           Result = {ok, Pid} | {error, Reason}
%% @doc Start top supervisor of the Yate application
%% @end
%%--------------------------------------------------------------------
start() ->
    application:start(dberl).


make() ->
    Modules = [
	       "dberl"
	      ],

    Prefix = "src/",
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
	       "remote_service",
	       "service",
	       "service_reg",
	       "sup",
	       "tcp_conn",
	       "transport"
	       ],
    Prefix2 = "src/dberl/",
    make_modules(Prefix2, Modules2).


make_modules(Prefix, Modules) ->
    Srcdir = "/home/mikael/svn/dberl/" ++ Prefix,
    Builddir = "/home/mikael/svn/dberl/build_linux/" ++ Prefix,
    Files = lists:map(fun(File) -> Srcdir ++ File end, Modules),

    make:files(Files,
	       [
		load,
		{i, "/home/mikael/svn/dberl/src"},
		{i, "/usr/lib/erlang/lib/xmerl-1.0.5/include"},
		{outdir, Builddir}
	       ]).


