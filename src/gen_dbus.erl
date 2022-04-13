%%
%% @copyright 2006-2007 Mikael Magnusson
%% @author Mikael Magnusson <mikma@users.sourceforge.net>
%% @doc dbus object behaviour
%%
-module(gen_dbus).

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

%% gen_server callback2
-export([
	 init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2
	]).

-callback init(tuple()) -> atom().
-callback handle_info(tuple(), term()) -> atom().

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
    case erlang:apply(Module, init, [Args]) of
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
			  ?debug("Methods: ~p~n", [Members]),
                          {Iface,
                           build_introspect(method, Members,
                                            State1, Interfaces)};
 		      {signals, Members} ->
 			  ?debug("Signals: ~p~n", [Members]),
 			  {Iface,
                           build_introspect(signal, Members,
                                            State1, Interfaces)};
		      _ ->
			  ?debug("Ignore config param ~p~n", [E]),
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
    ?debug("Node: ~p~n", [Node]),
    Xml_body = dbus_introspect:to_xml(Node),
    State2 = State1#state{default_iface=Iface,node=Node,xml_body=Xml_body},
    {ok, State2}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, State) ->
    ?debug("Unhandled call in: ~p~n", [Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({reply, From, Reply}, State) ->
    Pending = State#state.pending,
    case lists:keysearch(From, 1, Pending) of
	{value, {_, Message, Conn, Signature}} ->
	    ReplyMsg =
		case Reply of
		    {ok, ReplyBody} ->
			%% Send method return
			ReplyMsg1 = dbus_message:return(Message, Signature, [ReplyBody]),
			ReplyMsg1;
		    {dbus_error, Iface, Text} ->
			%% Send error
			ReplyMsg1 = dbus_message:error(Message, Iface, Text),
			ReplyMsg1;
		    _ ->
			?debug("Illegal reply ~p~n", [Reply]),
			ReplyMsg1 = dbus_message:error(Message, 'org.freedesktop.DBus.Error.Failed', "Failed"),
			ReplyMsg1
		end,
	    ok = dbus_connection:cast(Conn, ReplyMsg);
	false ->
	    ?debug("Pending not found: ~p ~p~n", [From, Pending]),
	    ignore
    end,
    Pending1 = lists:keydelete(From, 1, Pending),
    {noreply, State#state{pending=Pending1}};

handle_cast({signal, SignalName, Args, Options}, State) ->
    IfaceName =
	case lists:keysearch(interface, 1, Options) of
	    {value, {interface, Name}} ->
		Name;
	    false ->
		State#state.default_iface
	end,

    Signal = dbus_introspect:find_signal(State#state.node, IfaceName, SignalName),

    case Signal of
	{error, _}=Error ->
	    {reply, Error, State};
	_ ->
	    do_signal(IfaceName, Signal, Args, Options, State)
    end;

handle_cast(Request, State) ->
    ?error("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.

handle_info({dbus_method_call, Message, Conn}, State) ->
    Module = State#state.module,
    MemberStr = dbus_message:find_field(?FIELD_MEMBER, Message),
    Member = binary_to_atom(MemberStr, utf8),

    case Member of
	'Introspect' ->
	    ReplyBody = iolist_to_binary(State#state.xml_body),
	    ?debug("Introspect ~p ~p~n", [Message, ReplyBody]),
	    Reply = dbus_message:return(Message, [string], [ReplyBody]),
	    ok = dbus_connection:cast(Conn, Reply),
	    {noreply, State};

	_ ->
	    Sub = State#state.sub,

	    case catch do_method_call(Module, Member, Message, Conn, Sub) of
		{'EXIT', {undef, _}=Reason} ->
		    ?debug("undef method ~p~n", [Reason]),
		    ErrorName = "org.freedesktop.DBus.Error.UnknownMethod",
		    ErrorText = "Erlang: Function not found: " ++ MemberStr,
		    Reply = dbus_message:error(Message, ErrorName, ErrorText),
		    ok = dbus_connection:cast(Conn, Reply),
		    {noreply, State};
		{'EXIT', Reason} ->
 		    ?debug("Error ~p~n", [Reason]),
		    ErrorName = "org.freedesktop.DBus.Error.InvalidParameters",
		    ErrorText = "Erlang: Invalid parameters.",
		    Reply = dbus_message:error(Message, ErrorName, ErrorText),
 		    ?debug("InvalidParameters ~p~n", [Reply]),
		    ok = dbus_connection:cast(Conn, Reply),
		    {noreply, State};
		{ok, Sub1} ->
		    {noreply, State#state{sub=Sub1}};
		{pending, From, Sub1, Signature} ->
		    Pending = [{From, Message, Conn, Signature} |
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

do_signal(IfaceName, Signal, Args, _Options, State) ->
    Path = State#state.path,
    Message = dbus_message:signal(undefined, Path, IfaceName, Signal, Args),
    dbus_bus_reg:cast(Message),
    {noreply, State}.

do_method_call(Module, Member, Message = #dbus_message{header = Header}, Conn, Sub) ->
    From =
	if
	    Header#dbus_header.flags band ?NO_REPLY_EXPECTED ->
		none;
	    true ->
		make_ref()
	end,

    Signature =
        case lists:keysearch(signature, 1, erlang:apply(Module, Member, [dbus_info])) of
            {value, {signature, _Args, Returns}} ->
                Returns;
            false ->
                []
        end,

    Args =
        case Message#dbus_message.body of 
            undefined -> [];
            List when is_list(List) -> List;
            Other -> [Other]
        end,

    case {erlang:apply(Module, Member, [Args , {self(), From}, Sub]), From} of
	{{dbus_error, Iface, Msg, Sub1}, _} ->
	    Reply = dbus_message:error(Message, Iface, Msg),
	    ok = dbus_connection:cast(Conn, Reply),
	    {ok, Sub1};
	{{noreply, Sub1}, none} ->
	    {ok, Sub1};
	{{reply, _ReplyBody, Sub1}, none} ->
	    {ok, Sub1};
	{{reply, ReplyBody, Sub1}, _} ->
	    Reply = dbus_message:return(Message, Signature, [ReplyBody]),
	    ok = dbus_connection:cast(Conn, Reply),
	    {ok, Sub1};
	{{noreply, Sub1}, _} ->
	    {pending, From, Sub1, Signature}
    end.

build_introspect(MemberType, Members,
                 State, Interfaces) when is_list(Members),
                                         is_record(State, state) ->
    lists:foldl(fun(Member, Interfaces1) ->
                        member_build_introspect(MemberType, Member,
                                                State, Interfaces1)
                end, Interfaces, Members).

member_build_introspect(MemberType, Member,
                        State, Interfaces) when is_atom(Member),
                                                is_record(State, state) ->
    {InterfaceName, MemberName} = member_info(MemberType, Member, State),

    {Methods, Signals} =
        case dict:find(InterfaceName, Interfaces) of
            {ok, Value} ->
                Value;
            error ->
                {[], []}
        end,

    Interface1 =
        case MemberType of
            method ->
                {[MemberName | Methods], Signals};
            signal ->
                {Methods, [MemberName | Signals]}
        end,

    dict:store(InterfaceName, Interface1, Interfaces).


member_info(MemberType, Member, State) ->
    Info = erlang:apply(State#state.module, Member, [dbus_info]),
    InterfaceName =
	case lists:keysearch(interface, 1, Info) of
            {value, {interface, Interface1}} ->
                Interface1;
            false ->
                State#state.default_iface
        end,

    MemberName =
	case lists:keysearch(signature, 1, Info) of
	    {value, {signature, Args, Results}} ->
                ResultsArg =
                    case args_build_introspect(Results, out) of
                        [] ->
                            none;
                        [E] ->
                            E
                    end,
		ArgsXml = args_build_introspect(Args, in),

                case MemberType of
                    method ->
                        #dbus_method{name = Member,
                                args = ArgsXml,
                                result = ResultsArg};
                    signal ->
                        #dbus_signal{name = Member,
                                args = ArgsXml,
                                result = ResultsArg}
                end;
	    false ->
		undefined
	end,
    {InterfaceName, MemberName}.

args_build_introspect(Args, Dir) when is_list(Args) ->
    args_build_introspect(Args, Dir, []).

args_build_introspect([], _Dir, Acc) ->
    lists:reverse(Acc);
args_build_introspect([Arg | Rest], Dir, Acc) ->
    args_build_introspect(Rest, Dir, [arg_build_introspect(Arg, Dir)| Acc]).

arg_build_introspect(Arg, Dir) ->
    #dbus_arg{direction=Dir,
	 type=dbus_marshaller:marshal_signature(Arg)}.
