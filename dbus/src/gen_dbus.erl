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
	  node
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
	    Signal = #signal{name = 'OnClick',
			     out_sig="ss",
			     out_types=[string,string]},
	    Interface = #interface{name = 'org.designfu.SampleInterface',
				   signals = [Signal]},
	    Node = #node{interfaces = [Interface]},

	    State = #state{service=Service,
			   path=Path,
			   module=Module,
			   sub=SubState,
			   node=Node},
	    setup(DBus_config, State)
    end.

setup(DBus_config, State) ->
    Fun = fun(E, State2) ->
		  case E of
		      {interface, IFace} ->
			  State2#state{default_iface = IFace};
		      {members, Members} ->
			  State2#state{node = build_introspect(Members, State2)};
		      _ ->
			  io:format("Ignore config param ~p~n", [E]),
			  State2
		  end
	  end,

    State1 = lists:foldl(Fun, State, DBus_config),
    {ok, State1}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, State) ->
    error_logger:error_msg("Unhandled call in ~p: ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({reply, From, Reply}, State) ->
    Pending = State#state.pending,
    case lists:keysearch(From, 1, Pending) of
	{value, {_, Header, Conn}} ->
	    ReplyMsg =
		case Reply of
		    {ok, ReplyBody} ->
			%% Send method return
			{ok, ReplyMsg1} = dbus_message:build_method_return(Header, [string], [ReplyBody]),
			ReplyMsg1;
		    {dbus_error, Iface, Text} ->
			%% Send error
			{ok, ReplyMsg1} = dbus_message:build_error(Header, Iface, Text),
			ReplyMsg1;
		    _ ->
			error_logger:info_msg("Illegal reply ~p~n", [Reply]),
			{ok, ReplyMsg1} = dbus_message:build_error(Header, 'org.freedesktop.DBus.Error.Failed', "Failed"),
			ReplyMsg1
		end,
	    ok = dbus_connection:cast(Conn, ReplyMsg);
	false ->
	    error_logger:info_msg("Pending not found ~p: ~p ~p~n", [?MODULE, From, Pending]),
	    ignore
    end,
    Pending1 = lists:keydelete(From, 1, Pending),
    {noreply, State#state{pending=Pending1}};

handle_cast({signal, Signal_name, Args, Options}, State) ->
    io:format("in gen_server signal ~p~n", [Signal_name]),

    Iface_name =
	case lists:keysearch(interface, 1, Options) of
	    {value, {interface, Name}} ->
		Name;
	    _ ->
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
    %%Service = State#state.service,
    {_, MemberVar} = dbus_message:header_fetch(?HEADER_MEMBER, Header),
    MemberStr = MemberVar#variant.value,
    Member = list_to_atom(MemberStr),

%%     io:format("Handle call ~p ~p~n", [Header, Member]),
    case Member of
	'Introspect' ->
	    %% Empty introspect xml
	    ReplyBody = "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\" \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\"><node></node>",
	    {ok, Reply} = dbus_message:build_method_return(Header, [string], [ReplyBody]),
	    ok = dbus_connection:cast(Conn, Reply),
	    {noreply, State};

	_ ->
	    Sub = State#state.sub,

	    case catch do_method_call(Module, Member, Header, Conn, Sub) of
		{'EXIT', {undef, _}=Reason} ->
		    io:format("undef method ~p~n", [Reason]),
		    ErrorName = "org.freedesktop.DBus.Error.UnknownMethod",
		    ErrorText = "Erlang: Function not found: " ++ MemberStr,
		    {ok, Reply} = dbus_message:build_error(Header, ErrorName, ErrorText),
%% 		    io:format("Reply ~p~n", [Reply]),
		    ok = dbus_connection:cast(Conn, Reply),
		    {noreply, State};
		{'EXIT', Reason} ->
 		    io:format("Error ~p~n", [Reason]),
		    ErrorName = "org.freedesktop.DBus.Error.InvalidParameters",
		    ErrorText = "Erlang: Invalid parameters.",
		    {ok, Reply} = dbus_message:build_error(Header, ErrorName, ErrorText),
 		    io:format("InvalidParameters ~p~n", [Reply]),
		    ok = dbus_connection:cast(Conn, Reply),
		    {noreply, State};
		{ok, Sub1} ->
		    {noreply, State#state{sub=Sub1}};
		{pending, From, Sub1} ->
%% 		    io:format("Pending ~p~n", [From]),
		    Pending = [{From, Header, Conn}, State#state.pending],
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

do_signal(Iface_name, Signal, Args, Options, State) ->
    Path = State#state.path,
    {ok, Header} = dbus_message:build_signal(Path, Iface_name, Signal, Args),
    dbus_bus_reg:cast(Header),
    error_logger:error_msg("~p: signal ~p~n", [?MODULE, Header]),
    {noreply, State}.

do_method_call(Module, Member, Header, Conn, Sub) ->
    From =
	if
	    Header#header.flags band ?NO_REPLY_EXPECTED ->
		none;
	    true ->
		make_ref()
	end,

    case {Module:Member(Header#header.body, {self(), From}, Sub), From} of
	{{dbus_error, Iface, Msg, Sub1}, _} ->
	    {ok, Reply} = dbus_message:build_error(Header, Iface, Msg),
	    ok = dbus_connection:cast(Conn, Reply),
	    {ok, Sub1};
	{{noreply, Sub1}, none} ->
	    {ok, Sub1};
	{{reply, _ReplyBody, Sub1}, none} ->
%% 	    io:format("Ignore reply ~p~n", [ReplyBody]),
	    {ok, Sub1};
	{{reply, ReplyBody, Sub1}, _} ->
%% 	    io:format("Reply ~p~n", [ReplyBody]),
	    {ok, Reply} = dbus_message:build_method_return(Header, [variant], [ReplyBody]),
%% 	    io:format("Reply ~p~n", [Reply]),
	    ok = dbus_connection:cast(Conn, Reply),
	    {ok, Sub1};
	{{noreply, Sub1}, _} ->
	    {pending, From, Sub1}
    end.

build_introspect(Members, State) when is_list(Members),
				      is_record(State, state) ->
    #node{interfaces = [#interface{name = 'org.designfu.SampleInterface',
				   signals = [#signal{name = 'OnClick',
						      out_sig="ss",
						      out_types=[string,string]}]}]}.

%%     lists:map(fun(Member) -> member_build_introspect(Member) end, Members).

%% member_build_introspect(Member, State) when is_atom(Member),
%% 					    is_record(State, state) ->
%%     Module = State#state.module,
%%     Info = Module:Member(dbus_info),
%%     Signature = case lists:keysearch(signature, 1, Info) of
%% 		    {value, {signature, Result, Args}} ->
%% 			todo;
%% 		    _ ->
			
%% 		end.
