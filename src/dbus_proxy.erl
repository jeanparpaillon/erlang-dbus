%%
%% @copyright 2006-2007 Mikael Magnusson, 2014-2016 Jean Parpaillon
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc proxy representing a remote D-BUS object
%%
-module(dbus_proxy).

-include("dbus_client.hrl").
-include("dbus_dbus.hrl").


-behaviour(gen_server).

%% api
-export([
         start_link/2,
         start_link/3,
         start_link/4,
         stop/1,
         call/2,
         call/3,
         call/4,
         call/5,
         cast/2,
         cast/3,
         cast/4,
         connect_signal/2,
         connect_signal/4,
         connect_signal/6,
         has_interface/2,
         get_unique_name/1,
         interface/2,
         children/1
        ]).

%% gen_server callbacks
-export([init/1,
         code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-callback signal_handler(Sender    :: dbus_name(),
                         IfaceName :: dbus_name(),
                         Signal    :: dbus_name(),
                         Path      :: binary(),
                         Args      :: [dbus_arg()],
                         Ctx       :: any()) -> ok.

-record(signal_handler, {
          sender      :: dbus_name(),
          interface   :: dbus_name(),
          member      :: dbus_name(),
          path        :: {binary(), boolean()},
          mfa         :: handler()
         }).

-record(state, {
          service,              % atom() | string()
          path,                 % atom() | string()
          node            :: dbus_node(),   % #node()
          conn            :: dbus_connection(),
          waiting   = [],
          handlers  = []  :: [#signal_handler{}],
          uniquename      :: dbus_name()
         }).

-define(TIMEOUT, 5000).

-type t() :: dbus_proxy().
-type handler() :: mfa() | {fun(), any()} | pid().
-export_type([t/0, handler/0]).


%% @equiv start_link(Conn, Service, <<"/">>)
%% @end
-spec start_link(Conn :: dbus_connection(), Service :: dbus_name()) ->
            {ok, dbus_proxy()} | {error, term()}.
start_link(Conn, Service) ->
    gen_server:start_link(?MODULE, [Conn, Service, <<"/">>], []).


%% @doc Connect to an object, and introspect it.
%% @end
-spec start_link(Conn :: dbus_connection(), Service :: dbus_name(), Path :: binary() | atom()) ->
            {ok, dbus_proxy()} | {error, term()}.
start_link(Conn, Service, Path) ->
    gen_server:start_link(?MODULE, [Conn, Service, Path], []).


%% @doc Connect to an object, with known interfaces.
%% @end
-spec start_link(Conn :: dbus_connection(), Service :: dbus_name(), Path :: binary() | atom(), Node :: dbus_node()) ->
            {ok, dbus_proxy()} | {error, term()}.
start_link(Conn, Service, Path, #dbus_node{}=Node) ->
    gen_server:start_link(?MODULE, [Conn, Service, Path, Node], []).


%% @doc Disconnect proxy
%% @end
-spec stop(dbus_proxy()) -> ok.
stop(Proxy) ->
    gen_server:cast(Proxy, stop).


%% @equiv call(Proxy, Msg, 5000)
%% @end
-spec call(Proxy :: dbus_proxy(), Msg :: dbus_message()) -> {ok, term()} | {error, term()}.
call(Proxy, #dbus_message{}=Msg) ->
    call(Proxy, Msg, ?TIMEOUT).


%% @doc Sync send an arbitrary message
%% @end
-spec call(Proxy :: dbus_proxy(), Msg :: dbus_message(),
	   Timeout :: integer() | infinity) -> {ok, term()} | {error, term()}.
call(Proxy, #dbus_message{}=Msg, Timeout) when is_pid(Proxy) ->
    gen_server:call(Proxy, {call, Msg}, Timeout);
call({interface, Proxy, IfaceName}, MethodName, Args) when is_pid(Proxy) ->
    call(Proxy, IfaceName, MethodName, Args, ?TIMEOUT).


%% @equiv call(Proxy, Ifacename, MethodName, Args, 5000)
%% @end
-spec call(Proxy :: dbus_proxy(), IfaceName :: dbus_name(), MethodName :: dbus_name(), Args :: term()) ->
          ok | {ok, term()} | {error, term()}.
call({interface, Proxy, IfaceName}, MethodName, Args, Timeout) when is_pid(Proxy) ->
    call(Proxy, IfaceName, MethodName, Args, Timeout);
call(Proxy, IfaceName, MethodName, Args) when is_pid(Proxy) ->
    call(Proxy, IfaceName, MethodName, Args, ?TIMEOUT).

%% @doc Sync call a method
%% @end
-spec call(Proxy :: dbus_proxy(), IfaceName :: dbus_name(), MethodName :: dbus_name(), Args :: term(),
	   Timeout :: integer() | infinity) ->
          ok | {ok, term()} | {error, term()}.
call(Proxy, IfaceName, MethodName, Args, Timeout) when is_pid(Proxy) ->
    may_throw(gen_server:call(Proxy, {method, IfaceName, MethodName, Args}, Timeout)).


%% @doc Async send a message
%% @end
-spec cast(Proxy :: dbus_proxy(), Msg :: dbus_message()) -> ok | {error, term()}.
cast(Proxy, #dbus_message{}=Msg) ->
    gen_server:call(Proxy, {cast, Msg}).

%% @doc Async call a method
%% @end
-spec cast(Interface :: {interface, dbus_proxy(), dbus_name()}, MethodName :: dbus_name(), Args :: term()) -> ok.
cast({interface, Proxy, IfaceName}, MethodName, Args) ->
    cast(Proxy, IfaceName, MethodName, Args).

%% @doc Async call a method
%% @end
-spec cast(Proxy :: dbus_proxy(), IfaceName :: dbus_name(), MethodName :: dbus_name(), Args :: term()) -> ok.
cast(Proxy, IfaceName, MethodName, Args) ->
    gen_server:cast(Proxy, {method, IfaceName, MethodName, Args}).


%% @doc Get children of an object
%% @end
-spec children(Proxy :: dbus_proxy()) -> [binary()].
children(Proxy) ->
    gen_server:call(Proxy, children).

%% @doc Connect to every signal (eg for object manager)
%%
%% @todo Describe handlers
%% @end
-spec connect_signal(Proxy :: dbus_proxy(), Handler :: handler()) ->
                ok | {error, term()}.
connect_signal(Proxy, MFA) ->
    gen_server:call(Proxy, {connect_signal, MFA}).


%% @doc Connect to a particular signal
%%
%% @todo Describe handlers
%% @end
-spec connect_signal(Proxy :: dbus_proxy(),
             IfaceName :: dbus_name(),
             SignalName :: dbus_name(),
             Handler :: handler()) ->
                ok | {error, term()}.
connect_signal(Proxy, IfaceName, SignalName, MFA) ->
    gen_server:call(Proxy, {connect_signal, IfaceName, SignalName, MFA}).


%% @doc Connect to a particular signal on a particular children object
%% @end
-spec connect_signal(Proxy :: dbus_proxy(),
             Service :: dbus_name(),
             IfaceName :: dbus_name(),
             SignalName :: dbus_name(),
             Path :: binary(),
             MFA :: handler()) ->
                ok | {error, term()}.
connect_signal(Proxy, Service, IfaceName, SignalName, Path, MFA) ->
    gen_server:call(Proxy, {connect_signal, Service, IfaceName, SignalName, Path, MFA}).


%% @doc Check if object implements the given interface
%% @end
-spec has_interface(Proxy :: dbus_proxy(), InterfaceName :: dbus_name()) -> true | false.
has_interface(Proxy, InterfaceName) ->
    gen_server:call(Proxy, {has_interface, InterfaceName}).


%% @doc Check if object implements the given interface and return {ok, Iface} if true
%% @end
-spec interface(Proxy :: dbus_proxy(), InterfaceName :: dbus_name()) -> {ok, dbus_proxy(), dbus_name()} | {error, not_registered}.
interface(Proxy, InterfaceName) ->
    case has_interface(Proxy, InterfaceName) of
    true ->
      {ok, {interface, Proxy, InterfaceName}};
    false ->
      {error, not_registered}
    end
.

%% @doc Get the DBUS connection unique name.
%% @end
-spec get_unique_name(Proxy :: dbus_proxy()) -> {ok, binary()} | {error, term()}.
get_unique_name(Proxy) -> gen_server:call(Proxy, get_unique_name).


%%
%% gen_server callbacks
%%
init([Conn, Service0, Path]) ->
    Service = normalize_service_name(Service0),
    case do_introspect(Conn, Service, Path) of
    {ok, Node} ->
        Unique = do_unique_name(Conn, Service),
        {ok, #state{conn=Conn, service=Service, path=Path, node=Node, uniquename=Unique, handlers=[]}};
    {error, Err} ->
        ?error("Error introspecting object ~p: ~p~n", [Path, Err]),
        {stop, Err}
    end;

init([Conn, Service0, Path, Node]) ->
    Service = normalize_service_name(Service0),
    {ok, #state{conn=Conn, service=Service, path=Path, node=Node, handlers=[]}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-logging(debug).
handle_call({method, IfaceName, MethodName, Args}, _From, #state{node=Node}=State) ->
    ?debug("Calling ~p:~p.~p(~p)~n", [State#state.path, IfaceName, MethodName, Args]),
    case dbus_introspect:find_method(Node, IfaceName, MethodName) of
        {ok, Method} ->
            do_method(IfaceName, Method, Args, State);
        {error, _}=Err ->
            {reply, Err, State}
    end;

handle_call({connect_signal, Name, '_', '_', Path, MFA}, _From,
        #state{handlers=Handlers}=State) ->
    Match = [{type, signal},
	     {sender, Name},
	     {path_namespace, Path}],
    case do_method(?DBUS_IFACE, ?DBUS_DBUS_ADD_MATCH, [build_match(Match, <<>>)], State) of
	{reply, ok, S2} ->
	    Handler = #signal_handler{sender=Name, interface='_', member='_',
				      path={Path, true}, mfa=MFA},
	    {reply, ok, S2#state{handlers=[ Handler | Handlers ]}};
	{reply, {error, Err}, S2} -> {stop, {error, Err}, S2}
    end;

handle_call({connect_signal, Name, IfaceName, SignalName, Path, MFA}, _From,
	    #state{handlers=Handlers}=State) ->
    Match = [{type, signal},
	     {sender, Name},
	     {interface, IfaceName},
	     {member, SignalName},
	     {path, Path}],
    case do_method(?DBUS_IFACE, ?DBUS_DBUS_ADD_MATCH, [build_match(Match, <<>>)], State) of
        {reply, ok, S2} ->
            Handler = #signal_handler{sender=Name, interface=IfaceName, member=SignalName,
                                      path={Path, false}, mfa=MFA},
            {reply, ok, S2#state{handlers=[ Handler | Handlers ]}};
        {reply, {error, Err}, S2} -> {stop, {error, Err}, S2}
    end;

handle_call({connect_signal, MFA}, _From,
        #state{conn={dbus_bus_connection, Conn}, path=Path, uniquename=Name}=State) ->
    Ret = dbus_proxy:connect_signal(Conn, Name, '_', '_', Path, MFA),
    {reply, Ret, State};

handle_call({connect_signal, IfaceName, SignalName, MFA}, _From,
        #state{conn={dbus_bus_connection, Conn}, path=Path, uniquename=Name}=State) ->
    Ret = dbus_proxy:connect_signal(Conn, Name, IfaceName, SignalName, Path, MFA),
    {reply, Ret, State};

handle_call({has_interface, IfaceName}, _From, #state{node=Node}=State) ->
    case dbus_introspect:find_interface(Node, IfaceName) of
    {ok, _I} -> {reply, true, State};
    {error, _Err} -> {reply, false, State}
    end;

handle_call(children, _From, #state{node=#dbus_node{name=Name, elements=Children}}=State) ->
    Prefix = case Name of
                 undefined -> <<"/">>;
                 N -> N
             end,
    Paths = lists:map(fun (#dbus_node{name=ChildPath}) -> filename:join(Prefix, ChildPath) end, Children),
    {reply, Paths, State};

handle_call(get_unique_name, _From, #state{conn={dbus_peer_connection, PConn}}=State) ->
    Ret = dbus_peer_connection:get_unique_name(PConn),
    {reply, Ret, State};

handle_call({call, Msg}, _From, #state{conn=Conn}=State) ->
    Ret = dbus_connection:call(Conn, Msg),
    {reply, Ret, State};

handle_call({cast, Msg}, _From, #state{conn=Conn}=State) ->
    Ret = dbus_connection:cast(Conn, Msg),
    {reply, Ret, State};

handle_call(Request, _From, State) ->
    ?error("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Request, State) ->
    ?error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.

handle_info({dbus_signal, #dbus_message{header=Hdr, body=Args}}, #state{handlers=Handlers}=State) ->
    Sender = dbus_message:find_field(?FIELD_SENDER, Hdr),
    Path = dbus_message:find_field(?FIELD_PATH, Hdr),
    Iface = dbus_message:find_field(?FIELD_INTERFACE, Hdr),
    Signal = dbus_message:find_field(?FIELD_MEMBER, Hdr),
    case find_handlers({Sender, Iface, Signal, Path}, [], Handlers) of
        [] ->
            {noreply, State};
        Matches ->
            F = fun (Handler, Acc) ->
                        do_handle_signal(Handler, Acc, Sender, Iface, Signal, Path, Args)
                end,
            Handlers2 = lists:foldl(F, [], Matches),
            {noreply, State#state{handlers=Handlers2}}
    end;
handle_info({reply, #dbus_message{body=Body}, {tag, From, Options}}, State) ->
    reply(From, {ok, Body}, Options),
    {noreply, State};

handle_info({error, #dbus_message{body=Body}=Msg, {tag, From, Options}}, State) ->
    ErrName = dbus_message:get_field(?FIELD_ERROR_NAME, Msg),
    reply(From, {error, {ErrName, Body}}, Options),
    {noreply, State};

handle_info(Info, State) ->
    ?error("Unhandled info in ~p: ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, #state{conn=Conn}=_State) ->
    dbus_connection:close(Conn),
    terminated.

reply(From, Reply, Options) ->
    case lists:keysearch(reply, 1, Options) of
        {value, {reply, Pid, Ref}} ->
            Pid ! {reply, Ref, Reply};
        _ ->
            gen_server:reply(From, Reply)
    end,
    ok.

do_method(IfaceName, Method, Args, #state{service=Service, conn=Conn, path=Path}=State) ->
    Msg = dbus_message:call(Service, Path, IfaceName, Method),
    case dbus_message:set_body(Method, Args, Msg) of
        #dbus_message{}=M2 ->
            case dbus_connection:call(Conn, M2) of
                {ok, #dbus_message{body=undefined}} ->
                    {reply, ok, State};
                {ok, #dbus_message{body=Res}} ->
                    {reply, {ok, Res}, State};
                {error, #dbus_message{body=Body}=Ret} ->
                    Code = dbus_message:get_field(?FIELD_ERROR_NAME, Ret),
                    {reply, {throw, {binary_to_atom(Code, utf8), Body}}, State}
            end;
        {error, Err} ->
            {reply, {error, Err}, State}
    end.

may_throw({throw, What}) -> throw(What);
may_throw(AnyOther) -> AnyOther.

do_introspect(Conn, Service, Path) ->
    ?debug("Introspecting: ~p:~p~n", [Service, Path]),
    case dbus_connection:call(Conn, dbus_message:introspect(Service, Path)) of
        {ok, #dbus_message{body=Xml}} when is_binary(Xml) ->
            try dbus_introspect:from_xml_string(Xml, Path) of
                #dbus_node{}=Node -> {ok, Node}
            catch _:Err ->
                    ?error("Error parsing introspection infos: ~p~n", [Err]),
                    {error, parse_error}
            end;
        {ok, Msg} ->
            ?error("Error introspecting object: ~p", [Msg]),
            {error, invalid_introspect};
        {error, #dbus_message{body=Body}=Msg} ->
            Err = dbus_message:get_field(?FIELD_ERROR_NAME, Msg),
            {error, {Err, Body}}
    end.

do_unique_name(Conn, Service) ->
    Msg = dbus_message:call(?DBUS_SERVICE, ?DBUS_PATH, ?DBUS_IFACE, 'GetNameOwner'),
    M2 = dbus_message:set_body(?DBUS_DBUS_GET_NAME_OWNER, [Service], Msg),
    case dbus_connection:call(Conn, M2) of
        {ok, #dbus_message{body=Unique}} when is_binary(Unique) ->
            Unique;
        {ok, Msg} ->
            ?error("Error getting name owner: ~p", [Msg]),
            {error, invalid_nameowner};
        {error, #dbus_message{}=Err} ->
            case dbus_message:get_field(?FIELD_ERROR_NAME, Err) of
                <<"org.freedesktop.DBus.Error.NameHasNoOwner">> -> undefined;
                _ -> throw({error, Err})
            end
    end.

build_match([], << ",", Match/binary >>) ->
    Match;
build_match([{Key, Value} | Rules], Acc) when is_atom(Key) ->
    build_match([{atom_to_binary(Key, utf8), Value} | Rules], Acc);
build_match([{Key, Value} | Rules], Acc) when is_atom(Value) ->
    build_match([{Key, atom_to_binary(Value, utf8)} | Rules], Acc);
build_match([{Key, Value} | Rules], Acc) ->
    build_match(Rules, << Acc/binary, ",", Key/binary, "='", Value/binary, "'">>).


find_handlers(_Signal, Acc, []) ->
    Acc;
find_handlers(Signal, Acc, [ Handler | Tail ]) ->
    case match_handler(Handler, Signal) of
        true -> find_handlers(Signal, [ Handler | Acc ], Tail);
        false -> find_handlers(Signal, Acc, Tail)
    end.


match_handler({signal_handler, '_', '_', '_', '_',       _}, {_, _, _, _}) -> true;
match_handler({signal_handler, S,   '_', '_', '_',       _}, {S, _, _, _}) -> true;
match_handler({signal_handler, '_', I,   '_', '_',       _}, {_, I, _, _}) -> true;
match_handler({signal_handler, '_', '_', M,   '_',       _}, {_, _, M, _}) -> true;
match_handler({signal_handler, '_', '_', '_', PathMatch, _}, {_, _, _, P}) -> match_path(PathMatch, P);
match_handler({signal_handler, S,   I,   '_', '_',       _}, {S, I, _, _}) -> true;
match_handler({signal_handler, S,   '_', M,   '_',       _}, {S, _, M, _}) -> true;
match_handler({signal_handler, S,   '_', '_', PathMatch, _}, {S, _, _, P}) -> match_path(PathMatch, P);
match_handler({signal_handler, S,   I,   M,   '_',       _}, {S, I, M, _}) -> true;
match_handler({signal_handler, S,   I,   '_', PathMatch, _}, {S, I, _, P}) -> match_path(PathMatch, P);
match_handler({signal_handler, S,   I,   M,   PathMatch, _}, {S, I, M, P}) -> match_path(PathMatch, P);
match_handler(_,                                             _)            -> false.


match_path({P, _}, P) ->     true;
match_path({_, false}, _) -> false;
match_path({P, true}, NS) -> lists:prefix(filename:split(NS), filename:split(P)).


do_handle_signal(#signal_handler{mfa={Mod, Fun, Ctx}}=Handler, Acc, Sender, Iface, Signal, Path, Args) ->
    case erlang:function_exported(Mod, Fun, 6) of
        true ->
            try Mod:Fun(Sender, Iface, Signal, Path, Args, Ctx)
            catch Cls:Err ->
                    ?error("Error dispatching signal to ~p:~p/6: ~p:~p", [Mod, Fun, Cls, Err])
            end,
            [ Handler | Acc ];
        false -> Acc
    end;

do_handle_signal(#signal_handler{mfa={Fun, Ctx}}=Handler, Acc, Sender, Iface, Signal, Path, Args) ->
    try Fun(Sender, Iface, Signal, Path, Args, Ctx)
    catch Cls:Err ->
            ?error("Error dispatching signal to ~p/6: ~p:~p", [Fun, Cls, Err])
    end,
    [ Handler | Acc ];

do_handle_signal(#signal_handler{mfa=Pid}=Handler, Acc, Sender, Iface, Signal, Path, Args) when is_pid(Pid) ->
    Pid ! {signal, Sender, Iface, Signal, Path, Args},
    [ Handler | Acc ].

normalize_service_name(Name) when is_atom(Name) ->
  atom_to_binary(Name, utf8);
normalize_service_name(Name) when is_binary(Name) ->
  Name.
