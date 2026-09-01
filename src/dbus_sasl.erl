-module(dbus_sasl).
-moduledoc """
Codec for the D-Bus authentication protocol, the SASL profile described in
the "Authentication Protocol" section of the D-Bus specification
(`docs/specifications.html').

The protocol is line-based: every line is an all-caps command name, an
optional space and arguments, then CRLF. This module only encodes and
decodes those lines -- it holds no state and drives no conversation, which
is `dbus_auth_client_mech''s job. Being the client side, it *writes* the
client-to-server commands (`AUTH', `CANCEL', `BEGIN', `DATA', `ERROR',
`NEGOTIATE_UNIX_FD') and *reads* the server-to-client ones (`REJECTED',
`OK', `DATA', `ERROR', `AGREE_UNIX_FD').

Hex encoding of `DATA' payloads and of `AUTH' initial responses belongs to
the mechanisms, not here: what they hand over is copied to the wire as-is.
""".

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    parse/1,
    command_auth/0,
    command_auth/1,
    command_auth/2,
    command_data/1,
    command_begin/0,
    command_negotiate_unix_fd/0,
    command_cancel/0,
    command_error/0,
    command_error/1
]).

-type command() ::
    {data, binary()}
    | {rejected, [binary()]}
    | {ok, binary()}
    | {error, binary()}
    | agree_unix_fd
    | {unknown, binary()}.

-doc "Returns an AUTH command".
-spec command_auth() -> binary().
command_auth() ->
    <<"AUTH\r\n">>.

-spec command_auth(binary()) -> binary().
command_auth(Mech) ->
    <<"AUTH ", Mech/binary, "\r\n">>.

-spec command_auth(binary(), binary()) -> binary().
command_auth(Mech, <<>>) ->
    % The grammar has no way to spell an empty initial response -- a trailing
    % space is not an argument -- so it is the same line as no response at all.
    command_auth(Mech);
command_auth(Mech, InitialResponse) ->
    <<"AUTH ", Mech/binary, " ", InitialResponse/binary, "\r\n">>.

-spec command_data(binary()) -> binary().
command_data(<<>>) ->
    <<"DATA\r\n">>;
command_data(Data) ->
    <<"DATA ", Data/binary, "\r\n">>.

-spec command_begin() -> binary().
command_begin() ->
    <<"BEGIN\r\n">>.

-spec command_negotiate_unix_fd() -> binary().
command_negotiate_unix_fd() ->
    <<"NEGOTIATE_UNIX_FD\r\n">>.

-spec command_cancel() -> binary().
command_cancel() ->
    <<"CANCEL\r\n">>.

-spec command_error() -> binary().
command_error() ->
    <<"ERROR\r\n">>.

-spec command_error(binary()) -> binary().
command_error(<<>>) ->
    command_error();
command_error(Reason) ->
    <<"ERROR ", Reason/binary, "\r\n">>.

-doc """
Parses one server-to-client line.

Anything else -- a client-to-server command, an unknown or future command,
a malformed or unterminated line -- is returned verbatim as
`{unknown, Line}' rather than raising: the specification requires that an
unrecognised command be answered with `ERROR' and not be treated as fatal,
which is a decision for the state machine to make, not for the codec.
""".
-spec parse(binary()) -> command().
parse(Line) ->
    case line_body(Line) of
        {ok, Body} ->
            case command(Body) of
                unknown -> {unknown, Line};
                Command -> Command
            end;
        error ->
            {unknown, Line}
    end.

%%%
%%% Priv
%%%

%% One command is one line: it ends with CRLF, nothing follows that CRLF, and
%% every byte before it is printable ASCII -- the protocol is ASCII-only, and
%% a nul byte anywhere but the very first byte of the stream is an error.
line_body(Bin) when byte_size(Bin) >= 2 ->
    Len = byte_size(Bin) - 2,
    case Bin of
        <<Body:Len/binary, "\r\n">> ->
            case is_ascii(Body) of
                true -> {ok, Body};
                false -> error
            end;
        _ ->
            error
    end;
line_body(_Bin) ->
    error.

is_ascii(<<>>) ->
    true;
is_ascii(<<C, Rest/binary>>) when C >= 16#20, C =< 16#7E ->
    is_ascii(Rest);
is_ascii(_Bin) ->
    false.

%% Command names are matched case-sensitively, as the specification requires.
command(<<"OK">>) ->
    % Servers always send their GUID; an argument-less OK is accepted anyway,
    % the state machine only cares that the command is OK.
    {ok, <<>>};
command(<<"OK ", Rest/binary>>) ->
    case args(Rest) of
        [Guid] -> {ok, Guid};
        _ -> unknown
    end;
command(<<"REJECTED">>) ->
    {rejected, []};
command(<<"REJECTED ", Rest/binary>>) ->
    {rejected, args(Rest)};
command(<<"DATA">>) ->
    {data, <<>>};
command(<<"DATA ", Rest/binary>>) ->
    case args(Rest) of
        [Data] -> {data, Data};
        _ -> unknown
    end;
command(<<"ERROR">>) ->
    {error, <<>>};
command(<<"ERROR ", Rest/binary>>) ->
    % The argument is free-form human-readable text: kept verbatim.
    {error, Rest};
command(<<"AGREE_UNIX_FD">>) ->
    agree_unix_fd;
command(_Body) ->
    unknown.

%% Space-separated argument list. Empty runs are dropped rather than turned
%% into empty arguments, so a doubled or trailing space is tolerated.
args(Bin) ->
    [Arg || Arg <- binary:split(Bin, <<" ">>, [global]), Arg =/= <<>>].

-ifdef(TEST).

%%% The contract these tests pin, per the "Authentication Protocol" section
%%% of docs/specifications.html:
%%%
%%%   * every command is one line, `<NAME>[ <args>]\r\n', all-caps, ASCII
%%%     only, and case-sensitive;
%%%   * the command_*/N functions build the client-to-server commands byte
%%%     for byte, with no interpretation of their arguments -- hex encoding
%%%     is the mechanism's business;
%%%   * parse/1 recognises exactly the five server-to-client commands
%%%     (REJECTED, OK, DATA, ERROR, AGREE_UNIX_FD) and answers
%%%     `{unknown, Line}' for everything else, including future
%%%     `EXTENSION_' commands, rather than raising -- the specification
%%%     requires an unknown command be answered with ERROR, not be fatal.

%%%
%%% Client-to-server commands
%%%

command_auth_test_() ->
    [
        ?_assertEqual(<<"AUTH\r\n">>, command_auth()),
        ?_assertEqual(<<"AUTH EXTERNAL\r\n">>, command_auth(<<"EXTERNAL">>)),
        ?_assertEqual(
            <<"AUTH EXTERNAL 31303030\r\n">>,
            command_auth(<<"EXTERNAL">>, <<"31303030">>)
        ),
        %% no way to spell an empty initial response: same line as none
        ?_assertEqual(<<"AUTH ANONYMOUS\r\n">>, command_auth(<<"ANONYMOUS">>, <<>>))
    ].

command_data_test_() ->
    [
        ?_assertEqual(<<"DATA 8799cabb2ea93e\r\n">>, command_data(<<"8799cabb2ea93e">>)),
        ?_assertEqual(<<"DATA\r\n">>, command_data(<<>>))
    ].

command_error_test_() ->
    [
        ?_assertEqual(<<"ERROR\r\n">>, command_error()),
        ?_assertEqual(<<"ERROR\r\n">>, command_error(<<>>)),
        %% free-form text, spaces and all
        ?_assertEqual(
            <<"ERROR Not supported on this OS\r\n">>,
            command_error(<<"Not supported on this OS">>)
        )
    ].

command_no_argument_test_() ->
    [
        ?_assertEqual(<<"BEGIN\r\n">>, command_begin()),
        ?_assertEqual(<<"CANCEL\r\n">>, command_cancel()),
        ?_assertEqual(<<"NEGOTIATE_UNIX_FD\r\n">>, command_negotiate_unix_fd())
    ].

%% Whatever the mechanism hands over reaches the wire untouched -- the codec
%% neither hex-encodes nor validates it.
command_arguments_are_opaque_test_() ->
    [
        ?_assertEqual(<<"AUTH FOO_BAR\r\n">>, command_auth(<<"FOO_BAR">>)),
        ?_assertEqual(<<"DATA zzz\r\n">>, command_data(<<"zzz">>))
    ].

%%%
%%% Server-to-client commands
%%%

%% One entry per command form the server may send, with what it parses into.
server_commands() ->
    [
        {<<"OK 1234deadbeef\r\n">>, {ok, <<"1234deadbeef">>}},
        {<<"OK\r\n">>, {ok, <<>>}},
        {<<"REJECTED\r\n">>, {rejected, []}},
        {<<"REJECTED EXTERNAL\r\n">>, {rejected, [<<"EXTERNAL">>]}},
        {<<"REJECTED KERBEROS_V4 SKEY\r\n">>, {rejected, [<<"KERBEROS_V4">>, <<"SKEY">>]}},
        {<<"REJECTED EXTERNAL DBUS_COOKIE_SHA1 ANONYMOUS\r\n">>,
            {rejected, [<<"EXTERNAL">>, <<"DBUS_COOKIE_SHA1">>, <<"ANONYMOUS">>]}},
        {<<"DATA\r\n">>, {data, <<>>}},
        {<<"DATA 8799cabb2ea93e\r\n">>, {data, <<"8799cabb2ea93e">>}},
        {<<"ERROR\r\n">>, {error, <<>>}},
        {<<"ERROR Not supported on this OS\r\n">>, {error, <<"Not supported on this OS">>}},
        {<<"AGREE_UNIX_FD\r\n">>, agree_unix_fd}
    ].

parse_test_() ->
    [
        {binary_to_list(Line), ?_assertEqual(Parsed, parse(Line))}
     || {Line, Parsed} <- server_commands()
    ].

%% A mechanism list is space-separated; an extra or trailing space is
%% tolerated rather than yielding empty mechanism names.
parse_rejected_extra_spaces_test_() ->
    [
        ?_assertEqual({rejected, [<<"EXTERNAL">>]}, parse(<<"REJECTED EXTERNAL \r\n">>)),
        ?_assertEqual(
            {rejected, [<<"EXTERNAL">>, <<"ANONYMOUS">>]},
            parse(<<"REJECTED EXTERNAL  ANONYMOUS\r\n">>)
        )
    ].

%%%
%%% Everything else parses as {unknown, Line}
%%%

unknown_lines() ->
    [
        %% reserved for future extensions -- unknown here, but not fatal
        <<"EXTENSION_COM_MYDOMAIN_DO_STUFF\r\n">>,
        <<"FOOBAR\r\n">>,
        %% client-to-server commands: a client never receives these
        <<"AUTH EXTERNAL 31303030\r\n">>,
        <<"BEGIN\r\n">>,
        <<"CANCEL\r\n">>,
        <<"NEGOTIATE_UNIX_FD\r\n">>,
        %% the protocol is case-sensitive
        <<"ok 1234deadbeef\r\n">>,
        <<"Ok 1234deadbeef\r\n">>,
        <<"agree_unix_fd\r\n">>,
        %% a longer name that merely starts like a known one is another command
        <<"OKAY\r\n">>,
        <<"REJECTEDX\r\n">>,
        <<"AGREE_UNIX_FD_MAYBE\r\n">>,
        %% OK carries exactly one argument, the GUID; DATA one hex block;
        %% AGREE_UNIX_FD none at all
        <<"OK 1234deadbeef cafe\r\n">>,
        <<"DATA 8799cabb 2ea93e\r\n">>,
        <<"AGREE_UNIX_FD yes\r\n">>,
        %% only CRLF ends a line, and nothing may follow it
        <<"OK 1234deadbeef">>,
        <<"OK 1234deadbeef\r">>,
        <<"OK 1234deadbeef\n">>,
        <<"OK 1234deadbeef\r\nOK cafe\r\n">>,
        %% ASCII only, and no nul byte outside the initial credentials byte
        <<"ERROR caf", 16#C3, 16#A9, "\r\n">>,
        <<"OK 1234", 0, "deadbeef\r\n">>,
        %% not a command at all
        <<"\r\n">>,
        <<>>,
        <<0>>
    ].

parse_unknown_test_() ->
    [
        {binary_to_list(<<"unknown: ", Line/binary>>), ?_assertEqual({unknown, Line}, parse(Line))}
     || Line <- unknown_lines()
    ].

%%%
%%% DATA and ERROR travel in both directions, so they round-trip
%%%

round_trip_test_() ->
    [
        ?_assertEqual({data, <<"8799cabb2ea93e">>}, parse(command_data(<<"8799cabb2ea93e">>))),
        ?_assertEqual({data, <<>>}, parse(command_data(<<>>))),
        ?_assertEqual({error, <<>>}, parse(command_error())),
        ?_assertEqual({error, <<>>}, parse(command_error(<<>>))),
        ?_assertEqual(
            {error, <<"Not supported on this OS">>},
            parse(command_error(<<"Not supported on this OS">>))
        )
    ].

%%%
%%% The worked examples of the specification, figure by figure. "C:" lines
%%% are built, "S:" lines are parsed.
%%%

%% Figure 1. Successful EXTERNAL authentication.
external_example_test_() ->
    [
        ?_assertEqual(
            <<"AUTH EXTERNAL 31303030\r\n">>,
            command_auth(<<"EXTERNAL">>, <<"31303030">>)
        ),
        ?_assertEqual({ok, <<"1234deadbeef">>}, parse(<<"OK 1234deadbeef\r\n">>)),
        ?_assertEqual(<<"BEGIN\r\n">>, command_begin())
    ].

%% Figure 2. Finding out mechanisms, then picking one.
list_mechanisms_example_test_() ->
    [
        ?_assertEqual(<<"AUTH\r\n">>, command_auth()),
        ?_assertEqual(
            {rejected, [<<"KERBEROS_V4">>, <<"SKEY">>]},
            parse(<<"REJECTED KERBEROS_V4 SKEY\r\n">>)
        ),
        ?_assertEqual(<<"AUTH SKEY 7ab83f32ee\r\n">>, command_auth(<<"SKEY">>, <<"7ab83f32ee">>)),
        ?_assertEqual({data, <<"8799cabb2ea93e">>}, parse(<<"DATA 8799cabb2ea93e\r\n">>)),
        ?_assertEqual(
            <<"DATA 8ac876e8f68ee9809bfa876e6f9876g8fa8e76e98f\r\n">>,
            command_data(<<"8ac876e8f68ee9809bfa876e6f9876g8fa8e76e98f">>)
        ),
        ?_assertEqual({ok, <<"1234deadbeef">>}, parse(<<"OK 1234deadbeef\r\n">>))
    ].

%% Figure 3. An unknown command is answered with ERROR, then auth proceeds.
unknown_command_example_test_() ->
    [
        ?_assertEqual({unknown, <<"FOOBAR\r\n">>}, parse(<<"FOOBAR\r\n">>)),
        ?_assertEqual(<<"ERROR\r\n">>, command_error()),
        ?_assertEqual(
            <<"AUTH EXTERNAL 532d312d352d3138\r\n">>,
            command_auth(<<"EXTERNAL">>, <<"532d312d352d3138">>)
        )
    ].

%% Figure 4. Server does not support the initial mechanism -- an AUTH
%% naming a mechanism but carrying no initial response.
no_initial_response_example_test_() ->
    [
        ?_assertEqual(<<"AUTH EXTERNAL\r\n">>, command_auth(<<"EXTERNAL">>)),
        ?_assertEqual(
            {rejected, [<<"KERBEROS_V4">>, <<"SKEY">>]},
            parse(<<"REJECTED KERBEROS_V4 SKEY\r\n">>)
        )
    ].

%% Figures 5 and 6. A bare REJECTED ends the exchange, and CANCEL is
%% answered by one.
cancel_example_test_() ->
    [
        ?_assertEqual({rejected, []}, parse(<<"REJECTED\r\n">>)),
        ?_assertEqual(<<"CANCEL\r\n">>, command_cancel())
    ].

%% Figure 7. Unix fd passing negotiated successfully.
negotiate_unix_fd_example_test_() ->
    [
        ?_assertEqual(<<"NEGOTIATE_UNIX_FD\r\n">>, command_negotiate_unix_fd()),
        ?_assertEqual(agree_unix_fd, parse(<<"AGREE_UNIX_FD\r\n">>)),
        ?_assertEqual(<<"BEGIN\r\n">>, command_begin())
    ].

%% Figure 8. Unix fd passing refused: an ERROR with an explanation, which
%% the client answers with BEGIN all the same.
refuse_unix_fd_example_test_() ->
    [
        ?_assertEqual(<<"NEGOTIATE_UNIX_FD\r\n">>, command_negotiate_unix_fd()),
        ?_assertEqual(
            {error, <<"Not supported on this OS">>},
            parse(<<"ERROR Not supported on this OS\r\n">>)
        ),
        ?_assertEqual(<<"BEGIN\r\n">>, command_begin())
    ].

-endif.
