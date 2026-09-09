-module(prop_dbus_rpc).
-moduledoc """
Call laws for `dbus_rpc'.

`dbus_rpc:call/{2,3}' has three answers -- `ok', `{ok, Value}' and
`{error, Reason}' -- and which one a caller gets is decided before any bus is
involved: by the type of the message it was handed, by the `NO_REPLY_EXPECTED'
flag on it, and by what comes back on the connection. The laws below state that
decision:

1. a message that is not a `METHOD_CALL' is refused as `{error, {invalid_call,
   Message}}', from both arities, and nothing is sent -- a signal, a
   `METHOD_RETURN' and an `ERROR' are all things a caller may hold and none of
   them is answerable;
2. a call flagged `NO_REPLY_EXPECTED' answers `ok' as soon as the connection
   has taken it, and `{error, Reason}' when the connection refuses it. It never
   waits: the property gives it a 1 ms deadline, which a wait would blow;
3. a call that expects a reply answers `{ok, Body}' for a `METHOD_RETURN',
   `{error, dbus_error:t()}' for an `ERROR', and `{error, Reason}' when the
   send itself failed;
4. and `{error, timeout}' when nothing comes back.

The connection is a stub, not a `dbus_connection': `dbus_rpc' reaches it through
`dbus_connection:send/2', which is one `gen_server:call', and the reply it waits
for arrives as the `{dbus, Conn, Type, Serial, Message}' message
`dbus_connection' would have sent it. Nothing here marshals, so the generated
messages need only be well-formed records -- which is why law 1 can reuse
`dbus_marshaller_gen:message/0' with its header type overwritten, and why the
method calls of the other laws are built by `dbus_method_call:build/4' rather
than by hand: the flag under test is the one that constructor sets.
""".

-include_lib("proper/include/proper.hrl").
-include_lib("dbus/include/dbus.hrl").

-export([
    prop_non_method_call_rejected/0,
    prop_no_reply_expected/0,
    prop_reply_expected/0,
    prop_no_reply_times_out/0
]).

%% Nothing is ever sent to the caller on a connection scripted without a reply,
%% so the deadline cannot be lost to a race and its only job is to keep the
%% property's running time to the number of tests, rather than a multiple of the
%% 5 s default.
-define(SHORT_TIMEOUT, 1).

%%%
%%% 1. Only a METHOD_CALL is callable
%%%

prop_non_method_call_rejected() ->
    ?FORALL(
        Message,
        non_method_call(),
        with_conn(#{send => {ok, 1}}, fun(Conn) ->
            Expected = {error, {invalid_call, Message}},
            Expected =:= dbus_rpc:call(Conn, Message) andalso
                Expected =:= dbus_rpc:call(Conn, Message, ?SHORT_TIMEOUT) andalso
                0 =:= sends(Conn)
        end)
    ).

%%%
%%% 2. NO_REPLY_EXPECTED
%%%

prop_no_reply_expected() ->
    ?FORALL(
        {Call, Outcome},
        {method_call([no_reply_expected]), send_outcome()},
        begin
            Expected =
                case Outcome of
                    {ok, _Serial} -> ok;
                    {error, Reason} -> {error, Reason}
                end,
            with_conn(#{send => Outcome}, fun(Conn) ->
                Expected =:= dbus_rpc:call(Conn, Call, ?SHORT_TIMEOUT) andalso
                    1 =:= sends(Conn)
            end)
        end
    ).

%%%
%%% 3. A reply is expected
%%%

prop_reply_expected() ->
    ?FORALL(
        {Call, Serial, Outcome},
        {method_call([]), serial(), reply_outcome()},
        begin
            {Script, Expected} = script(Serial, Outcome),
            with_conn(Script, fun(Conn) -> Expected =:= dbus_rpc:call(Conn, Call) end)
        end
    ).

%%%
%%% 4. Nothing comes back
%%%

prop_no_reply_times_out() ->
    ?FORALL(
        {Call, Serial},
        {method_call([]), serial()},
        with_conn(#{send => {ok, Serial}}, fun(Conn) ->
            {error, timeout} =:= dbus_rpc:call(Conn, Call, ?SHORT_TIMEOUT)
        end)
    ).

%%%
%%% Generators
%%%

%% The three types a caller may hold that are not answerable. The type is read
%% off the header by `dbus_message:get_type/1', so overwriting that field is the
%% whole of what makes a generated message one of them.
non_method_call() ->
    ?LET(
        {{Message, _Decoded}, Type},
        {
            dbus_marshaller_gen:message(),
            union([?TYPE_SIGNAL, ?TYPE_METHOD_RETURN, ?TYPE_ERROR])
        },
        set_type(Type, Message)
    ).

set_type(Type, #dbus_message{header = Header} = Message) ->
    Message#dbus_message{header = Header#dbus_header{type = Type}}.

method_call(Options) ->
    ?LET(
        {Member, Path, InArgs},
        {
            dbus_marshaller_gen:value(string),
            dbus_marshaller_gen:value(object_path),
            dbus_marshaller_gen:sig_and_values()
        },
        dbus_method_call:build(Member, Path, InArgs, Options)
    ).

%% What `dbus_connection:send/2' answers: the serial it allocated, or the
%% reason it wrote nothing. The reasons are the ones the transport and the
%% connection actually produce.
send_outcome() ->
    union([
        {ok, serial()},
        {error, union([closed, enotconn, unix_fd_not_negotiated, unix_fd_not_supported])}
    ]).

serial() ->
    integer(1, 16#FFFFFFFF).

reply_outcome() ->
    union([
        {send_error, union([closed, enotconn])},
        {return, reply_body()},
        {error_reply, error_name(), error_body()}
    ]).

%% A script for the stub connection, and the answer `call/2' must give under it.
script(_Serial, {send_error, Reason}) ->
    {#{send => {error, Reason}}, {error, Reason}};
script(Serial, {return, Body}) ->
    {#{send => {ok, Serial}, reply => return_message(Serial, Body)}, {ok, Body}};
script(Serial, {error_reply, Name, Body}) ->
    {
        #{send => {ok, Serial}, reply => error_message(Serial, Name, Body)},
        {error, expected_error(Name, Body)}
    }.

%% The shape `dbus_marshaller:unmarshal_data/1' hands a body up in -- a single
%% value, a tuple of them, or nothing at all -- which is what a reply carries by
%% the time `dbus_rpc' sees it.
reply_body() ->
    ?LET(
        {Sig, Values},
        dbus_marshaller_gen:sig_and_values(),
        case dbus_marshaller_gen:canon_list(Sig, Values) of
            [] -> undefined;
            [One] -> One;
            Many -> list_to_tuple(Many)
        end
    ).

error_name() ->
    ?LET(
        Suffix,
        dbus_marshaller_gen:value(string),
        <<"org.freedesktop.DBus.Error.", Suffix/binary>>
    ).

%% An `ERROR' whose body is not a string is malformed and is generated on
%% purpose: `dbus_error:cast/1' keeps the name and drops it, so `call/2' still
%% answers rather than raising.
error_body() ->
    union([undefined, dbus_marshaller_gen:value(string), dbus_marshaller_gen:value(int32)]).

%% `dbus_error:cast/1' transcribed, so the law is stated against the message
%% that was generated rather than against the function that reads it.
expected_error(Name, undefined) -> Name;
expected_error(Name, Body) when is_binary(Body) -> {Name, Body};
expected_error(Name, _Body) -> Name.

%% Header fields as they come off the wire: `unmarshal_header_fields/2' unwraps
%% the `#dbus_variant{}' every field is encoded in.
return_message(Serial, Body) ->
    #dbus_message{
        header = #dbus_header{
            type = ?TYPE_METHOD_RETURN,
            serial = 1,
            fields = [{?FIELD_REPLY_SERIAL, Serial}]
        },
        body = Body
    }.

error_message(Serial, Name, Body) ->
    #dbus_message{
        header = #dbus_header{
            type = ?TYPE_ERROR,
            serial = 1,
            fields = [
                {?FIELD_ERROR_NAME, Name},
                {?FIELD_REPLY_SERIAL, Serial}
            ]
        },
        body = Body
    }.

%%%
%%% The stub connection
%%%
%%% A `dbus_connection:connection()' is a `gen_server:server_ref()' and
%%% `send/2' is one `gen_server:call' on it, so a bare process answering that
%%% call is a connection as far as `dbus_rpc' is concerned. This one answers
%%% what its script says, then delivers the script's reply -- if it has one --
%%% the way `dbus_connection:dispatch_return/4' does: straight to the caller's
%%% mailbox, tagged with the serial the send returned. It also counts the sends
%%% it was asked for, which is how law 1 sees that a refused call wrote nothing.

with_conn(Script, Fun) ->
    Conn = start_conn(Script),
    try
        Fun(Conn)
    after
        exit(Conn, kill),
        flush()
    end.

start_conn(Script) ->
    Owner = self(),
    spawn(fun() -> conn_loop(Owner, Script, 0) end).

conn_loop(Owner, #{send := Result} = Script, Sends) ->
    receive
        {'$gen_call', From, {send, _Type, _Message}} ->
            gen_server:reply(From, Result),
            deliver(Owner, Script),
            conn_loop(Owner, Script, Sends + 1);
        {'$gen_call', From, sends} ->
            gen_server:reply(From, Sends),
            conn_loop(Owner, Script, Sends)
    end.

deliver(Owner, #{send := {ok, Serial}, reply := Message}) ->
    Owner ! {dbus, self(), dbus_message:get_type(Message), Serial, Message},
    ok;
deliver(_Owner, _Script) ->
    ok.

sends(Conn) ->
    gen_server:call(Conn, sends).

%% A reply the call under test did not consume -- what a failing `wait_for_return'
%% would leave behind -- must not be there for the next test to find. Nothing
%% else is taken out of the mailbox.
flush() ->
    receive
        {dbus, _Conn, _Type, _Serial, _Message} -> flush()
    after 0 -> ok
    end.
