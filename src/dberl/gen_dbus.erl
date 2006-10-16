%%%
%%% dbus object behaviour
%%%

-module(dberl.gen_dbus).

-import(behaviour).
-import(error_logger).
-import(gen_server).
-import(io).
-import(lists).

-behaviour(gen_server).

-include("dbus.hrl").

%% api
-export([
	 start_link/5,
	 reply/2
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
	  pending=[]
	 }).

start_link(Service, Path, Module, Args, Options) ->
    gen_server:start_link(?MODULE, [Service, Path, Module, Args], Options).

reply({Self, From}, Reply) ->
    gen_server:cast(Self, {reply, From, Reply}).

%%
%% gen_server callbacks
%%
init([Service, Path, Module, Args]) ->
    case Module:init(Args) of
	{stop, Reason} ->
	    {stop, Reason};
	ignore ->
	    ignore;
	{ok, DBus_config, SubState} ->
	    ok = service:register_object(Service, Path, self()),
	    State = #state{service=Service,
			   path=Path,
			   module=Module,
			   sub=SubState},
	    setup(DBus_config, State)
    end.

setup(DBus_config, State) ->
    Fun = fun(E, State2) ->
		  case E of
		      {interface, IFace} ->
			  State2#state{default_iface = IFace};
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
			{ok, ReplyMsg1} = message:build_method_return(Header, [string], [ReplyBody]),
			ReplyMsg1;
		    {dbus_error, Iface, Text} ->
			%% Send error
			{ok, ReplyMsg1} = message:build_error(Header, Iface, Text),
			ReplyMsg1;
		    _ ->
			error_logger:info_msg("Illegal reply ~p~n", [Reply]),
			{ok, ReplyMsg1} = message:build_error(Header, 'org.freedesktop.DBus.Error.Failed', "Failed"),
			ReplyMsg1
		end,
	    ok = connection:cast(Conn, ReplyMsg);
	false ->
	    error_logger:info_msg("Pending not found ~p: ~p ~p~n", [?MODULE, From, Pending]),
	    ignore
    end,
    Pending1 = lists:keydelete(From, 1, Pending),
    {noreply, State#state{pending=Pending1}};

handle_cast(Request, State) ->
    error_logger:error_msg("Unhandled cast in ~p: ~p~n", [?MODULE, Request]),
    {noreply, State}.

handle_info({dbus_method_call, Header, Conn}, State) ->
    Module = State#state.module,
    %%Service = State#state.service,
    {_, MemberVar} = message:header_fetch(?HEADER_MEMBER, Header),
    Member = list_to_atom(MemberVar#variant.value),

    io:format("Handle call ~p ~p~n", [Header, Member]),
    case Member of
%% 	'Introspect' ->
%% 	    ReplyBody = "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\" \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\"><node><interface name=\"org.designfu.SampleInterface\"><method name=\"HelloWorld\"><arg direction=\"in\" type=\"i\" /><arg direction=\"in\" type=\"s\" /></method></interface></node>",
%% 	    {ok, Reply} = message:build_method_return(Header, [string], [ReplyBody]),
%% 	    io:format("Reply ~p~n", [Reply]),
%% 	    ok = connection:cast(Conn, Reply);

	_ ->
	    Sub = State#state.sub,

	    case catch do_method_call(Module, Member, Header, Conn, Sub) of
		{'EXIT', {undef, _}=Reason} ->
		    io:format("undef method ~p~n", [Reason]),
		    ErrorName = "org.freedesktop.DBus.Error.UnknownMethod",
		    ErrorText = "Erlang: Function not found.",
		    {ok, Reply} = message:build_error(Header, ErrorName, ErrorText),
		    io:format("Reply ~p~n", [Reply]),
		    ok = connection:cast(Conn, Reply),
		    {noreply, State};
		{'EXIT', _Reason} ->
		    ErrorName = "org.freedesktop.DBus.Error.InvalidParameters",
		    ErrorText = "Erlang: Object not found.",
		    {ok, Reply} = message:build_error(Header, ErrorName, ErrorText),
		    io:format("Reply ~p~n", [Reply]),
		    ok = connection:cast(Conn, Reply),
		    {noreply, State};
		{ok, Sub1} ->
		    {noreply, State#state{sub=Sub1}};
		{pending, From, Sub1} ->
		    io:format("Pending ~p~n", [From]),
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
	    {ok, Reply} = message:build_error(Header, Iface, Msg),
	    ok = connection:cast(Conn, Reply),
	    {ok, Sub1};
	{{noreply, Sub1}, none} ->
	    {ok, Sub1};
	{{reply, ReplyBody, Sub1}, none} ->
	    io:format("Ignore reply ~p~n", [ReplyBody]),
	    {ok, Sub1};
	{{reply, ReplyBody, Sub1}, _} ->
	    {ok, Reply} = message:build_method_return(Header, [string], [ReplyBody]),
	    io:format("Reply ~p~n", [Reply]),
	    ok = connection:cast(Conn, Reply),
	    {ok, Sub1};
	{{noreply, Sub1}, _} ->
	    {pending, From, Sub1}
    end.
