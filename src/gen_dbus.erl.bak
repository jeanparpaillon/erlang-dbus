%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc dbus object behaviour
%%
-module(gen_dbus).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

-include("dbus.hrl").

%% api
-export([
	 start_link/3,
	 start_link/4,
	 reply/2,
	 signal/2,
	 signal/3
	]).

%% behaviour callback
-export([behaviour_info/1]).

%% gen_server callback2
-export([
	 init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2
	]).

behaviour_info(callbacks) ->
    [
     {init, 1},
     {handle_info, 2}
    ].

-record(state, {
	  service,
	  module,
	  sub,
	  default_iface,
	  path,
	  pending=[],
	  node,
	  xml_body
	 }).

start_link(Module, Args, Options) ->
    gen_server:start_link(?MODULE, [Module, Args], Options).

start_link(ServerName, Module, Args, Options) ->
    gen_server:start_link(ServerName, ?MODULE, [Module, Args], Options).

reply({Self, From}, Reply) ->
    gen_server:cast(Self, {reply, From, Reply}).

signal(Signal, Args) ->
    signal(Signal, Args, []).

signal(Signal, Args, Options) ->
    gen_server:cast(self(), {signal, Signal, Args, Options}).

%%
%% gen_server callbacks
%%
init([Module, Args]) ->
    case Module:init(Args) of
	{stop, Reason} ->
	    {stop, Reason};
	ignore ->
	    ignore;
	{ok, {ServiceName, Path, DBus_config}, SubState} ->
	    {ok, Service} = dbus_service_reg:export_service(ServiceName),
	    ok = dbus_service:register_object(Service, Path, self()),
	    State = #state{service=Service,
			   path=Path,
			   module=Module,
			   sub=SubState},
	    setup(DBus_config, State)
    end.

setup(DBus_config, State) ->
    Default_face =
	case lists:keysearch(interface, 1, DBus_config) of
            {value, {interface, Interface}} ->
                Interface;
            false ->
                undefined
        end,

    State1 = State#state{default_iface=Default_face},

    Fun = fun(E, {Iface, Interfaces}) ->
		  case E of
		      {interface, Iface1} ->
                          %% Ignore
			  {Iface1, Interfaces};
		      {methods, Members} ->
			  lager:debug("Methods: ~p~n", [Members]),
                          {Iface,
                           build_introspect(method, Members,
                                            State1, Interfaces)};
 		      {signals, Members} ->
 			  lager:debug("Signals: ~p~n", [Members]),
 			  {Iface,
                           build_introspect(signal, Members,
                                            State1, Interfaces)};
		      _ ->
			  lager:debug("Ignore config param ~p~n", [E]),
			  {Iface, Interfaces}
		  end
	  end,

    {Iface, Interfaces} = lists:foldl(Fun, {undefined, dict:new()},
                                           DBus_config),
    Node = #dbus_node{name=State1#state.path,
                 interfaces=lists:map(fun({Key, {Methods, Signals}}) ->
                                              #dbus_iface{name=Key,
                                                         methods=Methods,
                                                         signals=Signals}
                                      end, dict:to_list(Interfaces))},
    lager:debug("Node: ~p~n", [Node]),
    Xml_body = dbus_introspect:to_xml(Node),
    State2 = State1#state{default_iface=Iface,node=Node,xml_body=Xml_body},
    {ok, State2}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, State) ->
    lager:debug("Unhandled call in: ~p~n", [Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({reply, From, Reply}, State) ->
    Pending = State#state.pending,
    case lists:keysearch(From, 1, Pending) of
	{value, {_, Header, Conn, Signature}} ->
	    ReplyMsg =
		case Reply of
		    {ok, ReplyBody} ->
			%% Send method return
			{ok, ReplyMsg1} = dbus_message:build_method_return(Header, Signature, [ReplyBody]),
			ReplyMsg1;
		    {dbus_error, Iface, Text} ->
			%% Send error
			{ok, ReplyMsg1} = dbus_message:build_error(Header, Iface, Text),
			ReplyMsg1;
		    _ ->
			lager:debug("Illegal reply ~p~n", [Reply]),
			{ok, ReplyMsg1} = dbus_message:build_error(Header, 'org.freedesktop.DBus.Error.Failed', "Failed"),
			ReplyMsg1
		end,
	    ok = dbus_connection:cast(Conn, ReplyMsg);
	false ->
	    lager:debug("Pending not found: ~p ~p~n", [From, Pending]),
	    ignore
    end,
    Pending1 = lists:keydelete(From, 1, Pending),
    {noreply, State#state{pending=Pending1}};

handle_cast({signal, Signal_name, Args, Options}, State) ->
    Iface_name =
	case lists:keysearch(interface, 1, Options) of
	    {value, {interface, Name}} ->
		Name;
	    false ->
		State#state.default_iface
	end,

    Signal =
	case dbus_introspect:find_interface(Iface_name, State#state.node) of
	    {ok, Iface} ->
		case dbus_introspect:find_signal(Signal_name, Iface) of
		    {ok, Signal1} ->
			Signal1;
		    error ->
			{error, {'org.freedesktop.DBus.UnknownSignal',  [Signal_name], Iface_name, State#state.node}}
		end;
	    error ->
		{error, {'org.freedesktop.DBus.UnknownInterface',  [Iface_name]}}
	end,

    case Signal of
	{error, _}=Error ->
	    {reply, Error, State}; 
	_ ->
	    do_signal(Iface_name, Signal, Args, Options, State)
    end;

handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.

handle_info({dbus_method_call, Header, Conn}, State) ->
    Module = State#state.module,
    {_, MemberVar} = dbus_message:header_fetch(?HEADER_MEMBER, Header),
    MemberStr = MemberVar#dbus_variant.value,
    Member = list_to_atom(MemberStr),

    case Member of
	'Introspect' ->
	    ReplyBody = State#state.xml_body,
	    lager:debug("Introspect ~p~n", [ReplyBody]),
	    {ok, Reply} = dbus_message:build_method_return(Header, [string], [ReplyBody]),
	    ok = dbus_connection:cast(Conn, Reply),
	    {noreply, State};

	_ ->
	    Sub = State#state.sub,

	    case catch do_method_call(Module, Member, Header, Conn, Sub) of
		{'EXIT', {undef, _}=Reason} ->
		    lager:debug("undef method ~p~n", [Reason]),
		    ErrorName = "org.freedesktop.DBus.Error.UnknownMethod",
		    ErrorText = "Erlang: Function not found: " ++ MemberStr,
		    {ok, Reply} = dbus_message:build_error(Header, ErrorName, ErrorText),
		    ok = dbus_connection:cast(Conn, Reply),
		    {noreply, State};
		{'EXIT', Reason} ->
 		    lager:debug("Error ~p~n", [Reason]),
		    ErrorName = "org.freedesktop.DBus.Error.InvalidParameters",
		    ErrorText = "Erlang: Invalid parameters.",
		    {ok, Reply} = dbus_message:build_error(Header, ErrorName, ErrorText),
 		    lager:debug("InvalidParameters ~p~n", [Reply]),
		    ok = dbus_connection:cast(Conn, Reply),
		    {noreply, State};
		{ok, Sub1} ->
		    {noreply, State#state{sub=Sub1}};
		{pending, From, Sub1, Signature} ->
		    Pending = [{From, Header, Conn, Signature} |
                               State#state.pending],
		    {noreply, State#state{sub=Sub1, pending=Pending}}
	    end
    end;

handle_info(Info, State) ->
    Module = State#state.module,
    Sub = State#state.sub,

    case Module:handle_info(Info, Sub) of
	{noreply, Sub1} ->
	    {noreply, State#state{sub=Sub1}};
	{stop, Reason, Sub1} ->
	    {stop, Reason, State#state{sub=Sub1}}
    end.


terminate(_Reason, _State) ->
    terminated.

do_signal(Iface_name, Signal, Args, _Options, State) ->
    Path = State#state.path,
    {ok, Header} = dbus_message:build_signal(Path, Iface_name, Signal, Args),
    dbus_bus_reg:cast(Header),
    lager:debug("signal ~p~n", [Header]),
    {noreply, State}.

do_method_call(Module, Member, Header, Conn, Sub) ->
    From =
	if
	    Header#dbus_header.flags band ?NO_REPLY_EXPECTED ->
		none;
	    true ->
		make_ref()
	end,

    Signature =
        case lists:keysearch(signature, 1, Module:Member(dbus_info)) of
            {value, {signature, _Args, Returns}} ->
                Returns;
            false ->
                []
        end,

    case {Module:Member(Header#dbus_header.fields , {self(), From}, Sub), From} of
	{{dbus_error, Iface, Msg, Sub1}, _} ->
	    {ok, Reply} = dbus_message:build_error(Header, Iface, Msg),
	    ok = dbus_connection:cast(Conn, Reply),
	    {ok, Sub1};
	{{noreply, Sub1}, none} ->
	    {ok, Sub1};
	{{reply, _ReplyBody, Sub1}, none} ->
	    {ok, Sub1};
	{{reply, ReplyBody, Sub1}, _} ->
	    {ok, Reply} = dbus_message:build_method_return(Header, Signature, [ReplyBody]),
	    ok = dbus_connection:cast(Conn, Reply),
	    {ok, Sub1};
	{{noreply, Sub1}, _} ->
	    {pending, From, Sub1, Signature}
    end.

build_introspect(Member_type, Members,
                 State, Interfaces) when is_list(Members),
                                         is_record(State, state) ->
    lists:foldl(fun(Member, Interfaces1) ->
                        member_build_introspect(Member_type, Member,
                                                State, Interfaces1)
                end, Interfaces, Members).

member_build_introspect(Member_type, Member,
                        State, Interfaces) when is_atom(Member),
                                                is_record(State, state) ->
    {Interface_name, Member_node} = member_info(Member_type, Member, State),

    {Methods, Signals} =
        case dict:find(Interface_name, Interfaces) of
            {ok, Value} ->
                Value;
            error ->
                {[], []}
        end,

    Interface1 =
        case Member_type of
            method ->
                {[Member_node | Methods], Signals};
            signal ->
                {Methods, [Member_node | Signals]}
        end,

    dict:store(Interface_name, Interface1, Interfaces).


member_info(Member_type, Member, State) ->
    Module = State#state.module,
    Info = Module:Member(dbus_info),
    Interface_name =
	case lists:keysearch(interface, 1, Info) of
            {value, {interface, Interface1}} ->
                Interface1;
            false ->
                State#state.default_iface
        end,

    Member_node =
	case lists:keysearch(signature, 1, Info) of
	    {value, {signature, Args, Results}} ->
                Results_arg =
                    case args_build_introspect(Results, out) of
                        [] ->
                            none;
                        [E] ->
                            E
                    end,
		Args_xml = args_build_introspect(Args, in),

                case Member_type of
                    method ->
                        #dbus_method{name = Member,
                                args = Args_xml,
                                result = Results_arg};
                    signal ->
                        #dbus_signal{name = Member,
                                args = Args_xml,
                                result = Results_arg}
                end;
	    false ->
		undefined
	end,
    {Interface_name, Member_node}.

args_build_introspect(Args, Dir) when is_list(Args) ->
    args_build_introspect(Args, Dir, []).

args_build_introspect([], _Dir, Acc) ->
    lists:reverse(Acc);
args_build_introspect([Arg | Rest], Dir, Acc) ->
    args_build_introspect(Rest, Dir, [arg_build_introspect(Arg, Dir)| Acc]).

arg_build_introspect(Arg, Dir) ->
    #dbus_arg{direction=Dir,
	 type=dbus_marshaller:marshal_signature(Arg)}.
