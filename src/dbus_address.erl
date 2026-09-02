-module(dbus_address).

-include("dbus.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([parse/1, escape/1, unescape/1]).
-export([scheme/1]).

-define(IS_LITERAL(C),
    ((C >= $0 andalso C =< $9) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $a andalso C =< $z) orelse
        C =:= $- orelse C =:= $_ orelse
        C =:= $/ orelse C =:= $. orelse C =:= $*)
).

-doc "Returns address' scheme".
-spec scheme(dbus_address()) -> scheme().
scheme(Address) ->
    Address#dbus_address.scheme.

-doc """
Parse a D-Bus address string into a list of `dbus_address()` records.
At this point, options are not validated against scheme.

`guid` is the exception: being a generic server attribute rather than a
transport parameter, it is lifted into its own field.
""".
-spec parse(binary()) -> {ok, [dbus_address()]} | {error, term()}.
parse(Addresses) when is_binary(Addresses) ->
    %% docs/addresses.md "Parsing Order": split the structure on literal
    %% delimiters first, percent-decode the values last. Splitting a decoded
    %% string would let an escaped delimiter act as a real one.
    parse_addresses(binary:split(Addresses, <<";">>, [global]), []);
parse(Addresses) ->
    {error, {badarg, Addresses}}.

parse_addresses([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_addresses([Address | Rest], Acc) ->
    case parse_address(Address) of
        {ok, BusId} -> parse_addresses(Rest, [BusId | Acc]);
        {error, _} = E -> E
    end.

%% `;' separates two addresses, so an empty element -- from a leading,
%% trailing or doubled `;', or from an empty input -- is a malformed list
%% rather than something to skip: silently dropping it would turn a typo
%% into a shorter list of alternatives.
parse_address(<<>>) ->
    {error, empty_address};
parse_address(Address) ->
    %% binary:split/2 without `global' splits on the FIRST occurrence, which
    %% is what the grammar wants: a `:' later on belongs to a value.
    case binary:split(Address, <<":">>) of
        [_] ->
            {error, {no_transport_delimiter, Address}};
        [<<>>, _] ->
            {error, {empty_transport, Address}};
        [TransportBinary, Params] ->
            case validate_transport(TransportBinary) of
                {error, _} = E ->
                    E;
                {ok, Transport} ->
                    case parse_params(Params) of
                        {ok, Options} ->
                            %% Verbatim, and left as a binary: this module
                            %% only checks syntax, and the format is
                            %% extensible, so deciding which names are
                            %% meaningful -- and interning them -- belongs to
                            %% whoever resolves a transport.
                            build(Transport, Options);
                        {error, _} = E ->
                            E
                    end
            end
    end.

%% `guid' is the one parameter every transport may carry -- "a server may
%% specify a key-value pair with the key guid", not a transport key -- so it
%% gets its own field rather than sitting in the per-transport options, and a
%% transport layer never has to know about it. It is not decoded further
%% here: whether the value really is 16 hex-encoded bytes is a question for
%% whoever compares GUIDs.
build(Scheme, Options) ->
    case lists:keytake(guid, 1, Options) of
        false ->
            {ok, #dbus_address{scheme = Scheme, options = Options}};
        {value, {guid, Guid}, Rest} ->
            case lists:keymember(guid, 1, Rest) of
                true ->
                    %% One field, so two values cannot both be kept and
                    %% there is no rule saying which wins.
                    {error, {duplicate_parameter, guid}};
                false ->
                    {ok, #dbus_address{scheme = Scheme, guid = Guid, options = Rest}}
            end
    end.

%% "an optional, comma-separated list of keys and values": `autolaunch:' and
%% `systemd:' carry none at all.
parse_params(<<>>) ->
    {ok, []};
parse_params(Params) ->
    parse_params(binary:split(Params, <<",">>, [global]), []).

parse_params([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_params([Param | Rest], Acc) ->
    case parse_param(Param) of
        {ok, Option} -> parse_params(Rest, [Option | Acc]);
        {error, _} = E -> E
    end.

parse_param(<<>>) ->
    {error, empty_parameter};
parse_param(Param) ->
    case binary:split(Param, <<"=">>) of
        [_] ->
            {error, {parameter_without_value, Param}};
        [<<>>, _] ->
            {error, {empty_parameter_key, Param}};
        [Key, Value] ->
            case validate_param(Key) of
                {error, _} = E ->
                    E;
                {ok, ParamKey} ->
                    case unescape(Value) of
                        {ok, Decoded} ->
                            {ok, {ParamKey, Decoded}};
                        {error, Reason} ->
                            {error, {ParamKey, Reason}}
                    end
            end
    end.

validate_transport(Bin) ->
    case is_name(Bin) of
        true -> {ok, Bin};
        false -> {error, {invalid_transport, Bin}}
    end.

validate_param(Bin) ->
    case is_name(Bin) of
        true ->
            try
                {ok, binary_to_existing_atom(Bin, utf8)}
            catch
                error:badarg -> {error, {invalid_parameter, Bin}}
            end;
        false ->
            {error, {invalid_parameter, Bin}}
    end.

%% Transport names and parameter keys are not escaped -- the spec escapes
%% values only -- so they have to be literal to begin with.
is_name(<<>>) ->
    false;
is_name(Name) ->
    is_literal(Name).

is_literal(<<>>) ->
    true;
is_literal(<<C, Rest/binary>>) when ?IS_LITERAL(C) ->
    is_literal(Rest);
is_literal(_) ->
    false.

-doc """
Escape a binary string for use in a D-Bus address.

Characters out of the valid set are escaped where %XX `XX` hexadecimal
representation of the character's byte value.

Valid set is:
```text
[-0-9A-Za-z_/.*]
```
""".
-spec escape(binary()) -> binary().
escape(Value) when is_binary(Value) ->
    escape(Value, <<>>);
escape(Value) ->
    erlang:error({badarg, Value}).

escape(<<>>, Acc) ->
    Acc;
escape(<<C, Rest/binary>>, Acc) when ?IS_LITERAL(C) ->
    escape(Rest, <<Acc/binary, C>>);
escape(<<C, Rest/binary>>, Acc) ->
    escape(Rest, <<Acc/binary, $%, (hex(C bsr 4)), (hex(C band 16#0f))>>).

hex(N) when N < 10 -> $0 + N;
hex(N) -> $A + N - 10.

-doc """
Unescape given binary string from a D-Bus address.

Characters in the form `%XX` are replaced with the corresponding byte value.
""".
-spec unescape(binary()) -> {ok, binary()} | {error, term()}.
unescape(Value) when is_binary(Value) ->
    unescape(Value, <<>>);
unescape(Value) ->
    {error, {badarg, Value}}.

%% Strict: the escaping rules say every byte outside the literal set is
%% written as %HH, so a value carrying one raw -- a space, or a structural
%% delimiter that survived the split -- is malformed rather than something to
%% pass through. That makes unescape/1 the validator for a single value, and
%% parse/1 inherits the check by calling it.
unescape(<<>>, Acc) ->
    {ok, Acc};
unescape(<<$%, H, L, Rest/binary>>, Acc) ->
    case {unhex(H), unhex(L)} of
        {Hi, Lo} when is_integer(Hi), is_integer(Lo) ->
            unescape(Rest, <<Acc/binary, ((Hi bsl 4) bor Lo)>>);
        _ ->
            {error, {invalid_escape, <<$%, H, L>>}}
    end;
unescape(<<$%, Rest/binary>>, _Acc) ->
    %% Fewer than two bytes left after the `%'.
    {error, {truncated_escape, <<$%, Rest/binary>>}};
unescape(<<C, Rest/binary>>, Acc) when ?IS_LITERAL(C) ->
    unescape(Rest, <<Acc/binary, C>>);
unescape(<<C, _Rest/binary>>, _Acc) ->
    {error, {unescaped_byte, C}}.

unhex(C) when C >= $0, C =< $9 -> C - $0;
unhex(C) when C >= $a, C =< $f -> C - $a + 10;
unhex(C) when C >= $A, C =< $F -> C - $A + 10;
unhex(_) -> error.

%%%
%%% eunit
%%%
-ifdef(TEST).

%%% The contract these tests pin, per docs/addresses.md:
%%%
%%%   parse(binary()) -> {ok, [#bus_id{}]} | {error, term()}
%%%
%%%   * one #bus_id{} per `;'-separated address, in the order written --
%%%     the addresses are alternatives, so order is significant;
%%%   * `scheme' is the transport name, verbatim, as a binary -- this
%%%     module checks syntax only, so nothing here says which names are
%%%     meaningful;
%%%   * `options' is an ordered proplist of {atom(), binary()}, one entry
%%%     per parameter as written, values percent-decoded;
%%%   * `guid' is lifted out of `options' into its own field -- it is a
%%%     generic server attribute rather than a transport parameter -- and is
%%%     `undefined' when the address does not carry one;
%%%   * no transport-specific interpretation happens here -- unknown
%%%     transports and unknown keys parse like any other, `port' stays a
%%%     binary, and nothing is rejected for being listen-only. That is
%%%     what "options are not validated against scheme" means above.

scheme_test() ->
    ?assertMatch(
        <<"unix">>,
        scheme(#dbus_address{scheme = <<"unix">>})
    ).

%%%
%%% Valid addresses
%%%

%% One entry per form named in docs/addresses.md, with the #bus_id{} it
%% must parse into. Drives the single-address tests, the address-list
%% tests, and the escaping tests below.
valid_addresses() ->
    [
        %% unix: -- connectable forms
        {<<"unix:path=/run/user/1000/bus">>, #dbus_address{
            scheme = <<"unix">>, options = [{path, <<"/run/user/1000/bus">>}]
        }},
        {<<"unix:abstract=/tmp/dbus-XYZ">>, #dbus_address{
            scheme = <<"unix">>, options = [{abstract, <<"/tmp/dbus-XYZ">>}]
        }},

        %% unix: -- listen-only forms, parsed but not interpreted
        {<<"unix:dir=/some/directory">>, #dbus_address{
            scheme = <<"unix">>, options = [{dir, <<"/some/directory">>}]
        }},
        {<<"unix:tmpdir=/tmp">>, #dbus_address{
            scheme = <<"unix">>, options = [{tmpdir, <<"/tmp">>}]
        }},
        {<<"unix:runtime=yes">>, #dbus_address{
            scheme = <<"unix">>, options = [{runtime, <<"yes">>}]
        }},

        %% tcp:
        {<<"tcp:host=127.0.0.1,port=12345">>, #dbus_address{
            scheme = <<"tcp">>,
            options = [
                {host, <<"127.0.0.1">>},
                {port, <<"12345">>}
            ]
        }},
        {<<"tcp:host=localhost,port=12345,family=ipv4">>, #dbus_address{
            scheme = <<"tcp">>,
            options = [
                {host, <<"localhost">>},
                {port, <<"12345">>},
                {family, <<"ipv4">>}
            ]
        }},

        %% nonce-tcp:
        {<<"nonce-tcp:host=localhost,port=12345,noncefile=/tmp/dbus-nonce">>, #dbus_address{
            scheme = <<"nonce-tcp">>,
            options = [
                {host, <<"localhost">>},
                {port, <<"12345">>},
                {noncefile, <<"/tmp/dbus-nonce">>}
            ]
        }},

        %% launchd:
        {<<"launchd:env=DBUS_LAUNCHD_SESSION_BUS_SOCKET">>, #dbus_address{
            scheme = <<"launchd">>,
            options = [{env, <<"DBUS_LAUNCHD_SESSION_BUS_SOCKET">>}]
        }},

        %% unixexec:
        {<<"unixexec:path=/usr/bin/example">>, #dbus_address{
            scheme = <<"unixexec">>, options = [{path, <<"/usr/bin/example">>}]
        }},
        {<<"unixexec:path=/usr/bin/example,argv1=foo,argv2=bar">>, #dbus_address{
            scheme = <<"unixexec">>,
            options = [
                {path, <<"/usr/bin/example">>},
                {argv1, <<"foo">>},
                {argv2, <<"bar">>}
            ]
        }},

        %% autolaunch: -- including the parameterless form
        {<<"autolaunch:">>, #dbus_address{scheme = <<"autolaunch">>, options = []}},
        {<<"autolaunch:scope=*user">>, #dbus_address{
            scheme = <<"autolaunch">>, options = [{scope, <<"*user">>}]
        }},

        %% systemd: -- listen-only, recognised by the syntax parser anyway
        {<<"systemd:">>, #dbus_address{scheme = <<"systemd">>, options = []}},

        %% guid is a generic server attribute, so it lands in its own field
        %% rather than among the transport's options
        {<<"unix:path=/run/user/1000/bus,guid=0123456789abcdef0123456789abcdef">>, #dbus_address{
            scheme = <<"unix">>,
            guid = <<"0123456789abcdef0123456789abcdef">>,
            options = [{path, <<"/run/user/1000/bus">>}]
        }},

        %% an unknown transport must NOT be rejected: the format is extensible
        {<<"future-transport:foo=bar,baz=quux">>, #dbus_address{
            scheme = <<"future-transport">>,
            options = [{foo, <<"bar">>}, {baz, <<"quux">>}]
        }},

        %% every character of the optionally-escaped set [-0-9A-Za-z_/.*]
        %% may appear literally
        {<<"unix:path=/a-b_c.d*e/0Z">>, #dbus_address{
            scheme = <<"unix">>, options = [{path, <<"/a-b_c.d*e/0Z">>}]
        }},

        %% only the FIRST `=' separates key from value, so a value may hold a
        %% further one -- escaped, per the rule pinned in
        %% value_with_raw_equals_test/0 below
        {<<"unixexec:path=/usr/bin/example,argv1=a%3Db">>, #dbus_address{
            scheme = <<"unixexec">>,
            options = [
                {path, <<"/usr/bin/example">>},
                {argv1, <<"a=b">>}
            ]
        }}
    ].

single_address_test_() ->
    [
        {binary_to_list(Addr), ?_assertEqual({ok, [Expected]}, parse(Addr))}
     || {Addr, Expected} <- valid_addresses()
    ].

%%%
%%% Address lists
%%%

%% The example from docs/addresses.md, spelled out rather than generated.
two_addresses_test() ->
    ?assertEqual(
        {ok, [
            #dbus_address{scheme = <<"unix">>, options = [{path, <<"/tmp/dbus">>}]},
            #dbus_address{
                scheme = <<"tcp">>,
                options = [
                    {host, <<"localhost">>},
                    {port, <<"12345">>}
                ]
            }
        ]},
        parse(<<"unix:path=/tmp/dbus;tcp:host=localhost,port=12345">>)
    ).

%% Any valid list of addresses parses into the corresponding list of
%% #bus_id{} records: the whole corpus, joined with `;', in order.
whole_corpus_as_one_list_test() ->
    {Addrs, Expected} = lists:unzip(valid_addresses()),
    ?assertEqual({ok, Expected}, parse(join(Addrs))).

%% ... and so does every ordered pair drawn from it, so that no address
%% depends on being first, last, or alone.
every_pair_test() ->
    lists:foreach(
        fun({{A1, E1}, {A2, E2}}) ->
            Joined = join([A1, A2]),
            ?assertEqual({ok, [E1, E2]}, parse(Joined))
        end,
        [{X, Y} || X <- valid_addresses(), Y <- valid_addresses()]
    ).

%% A single address is a one-element list, i.e. parse/1 never returns a
%% bare record for the degenerate case.
list_of_one_test() ->
    ?assertMatch({ok, [#dbus_address{}]}, parse(<<"unix:path=/run/user/1000/bus">>)).

join(Addrs) ->
    iolist_to_binary(lists:join(<<";">>, Addrs)).

%%%
%%% Percent escaping -- decoded AFTER the structural split
%%%

%% "/tmp/foo%2Cbar must produce path = /tmp/foo,bar and NOT two parameters"
escaped_comma_is_not_a_separator_test() ->
    ?assertEqual(
        {ok, [
            #dbus_address{
                scheme = <<"unix">>,
                guid = undefined,
                options = [{path, <<"/tmp/foo,bar">>}]
            }
        ]},
        parse(<<"unix:path=/tmp/foo%2Cbar">>)
    ).

escaped_space_test() ->
    ?assertEqual(
        {ok, [
            #dbus_address{
                scheme = <<"unix">>,
                guid = undefined,
                options = [{path, <<"/tmp/my bus">>}]
            }
        ]},
        parse(<<"unix:path=/tmp/my%20bus">>)
    ).

%% %3B %3A %3D %25 must not be read as structural delimiters
escaped_delimiters_are_not_structural_test() ->
    ?assertEqual(
        {ok, [
            #dbus_address{
                scheme = <<"unix">>,
                guid = undefined,
                options = [{path, <<";:=%">>}]
            }
        ]},
        parse(<<"unix:path=%3B%3A%3D%25">>)
    ).

%% An escaped `;' does not start a second address
escaped_semicolon_does_not_split_address_test() ->
    ?assertEqual(
        {ok, [
            #dbus_address{
                scheme = <<"unix">>,
                guid = undefined,
                options = [{path, <<"/tmp/a;b">>}]
            }
        ]},
        parse(<<"unix:path=/tmp/a%3Bb">>)
    ).

hex_digits_are_case_insensitive_test() ->
    Expected =
        {ok, [
            #dbus_address{
                scheme = <<"unix">>,
                guid = undefined,
                options = [{path, <<"/tmp/foo,bar">>}]
            }
        ]},
    ?assertEqual(Expected, parse(<<"unix:path=/tmp/foo%2cbar">>)),
    ?assertEqual(Expected, parse(<<"unix:path=/tmp/foo%2Cbar">>)).

%% Escapes are per-byte, so a multi-byte character is several escapes
escaped_utf8_value_test() ->
    ?assertEqual(
        {ok, [
            #dbus_address{
                scheme = <<"unix">>,
                guid = undefined,
                options = [{path, <<"/tmp/caf", 16#c3, 16#a9>>}]
            }
        ]},
        parse(<<"unix:path=/tmp/caf%C3%A9">>)
    ).

%% Escaping applies to values only; the transport name and the keys are
%% split out before any decoding happens.
escapes_only_in_values_test() ->
    ?assertEqual(
        {ok, [
            #dbus_address{
                scheme = <<"unix">>,
                guid = undefined,
                options = [{path, <<"/tmp/a=b">>}]
            }
        ]},
        parse(<<"unix:path=/tmp/a%3Db">>)
    ).

%%%
%%% Invalid addresses
%%%
%%% Only that they are rejected is pinned, not the reason term.

no_transport_delimiter_test() ->
    ?assertMatch({error, _}, parse(<<"unix">>)).

empty_transport_test() ->
    ?assertMatch({error, _}, parse(<<":path=/tmp/bus">>)).

parameter_without_value_test() ->
    ?assertMatch({error, _}, parse(<<"unix:path">>)).

empty_parameter_key_test() ->
    ?assertMatch({error, _}, parse(<<"unix:=/tmp/bus">>)).

empty_parameter_test() ->
    ?assertMatch({error, _}, parse(<<"unix:path=/tmp/bus,,guid=abc">>)).

truncated_escape_test() ->
    ?assertMatch({error, _}, parse(<<"unix:path=/tmp/bus%2">>)).

invalid_hex_escape_test() ->
    ?assertMatch({error, _}, parse(<<"unix:path=/tmp/bus%ZZ">>)).

%% A character outside [-0-9A-Za-z_/.*] must be escaped, not written literally
unescaped_space_test() ->
    ?assertMatch({error, _}, parse(<<"unix:path=/tmp/my bus">>)).

empty_address_test() ->
    ?assertMatch({error, _}, parse(<<>>)).

%% One bad alternative invalidates the list -- the client cannot know which
%% of the remaining ones the writer meant.
one_invalid_alternative_test() ->
    ?assertMatch(
        {error, _},
        parse(<<"unix:path=/tmp/dbus;notatransport">>)
    ).

%% `;' separates two addresses, so an empty element on either side of one
%% is a malformed list rather than something to skip over.
trailing_semicolon_test() ->
    ?assertMatch({error, _}, parse(<<"unix:path=/tmp/dbus;">>)).

leading_semicolon_test() ->
    ?assertMatch({error, _}, parse(<<";unix:path=/tmp/dbus">>)).

empty_alternative_test() ->
    ?assertMatch(
        {error, _},
        parse(<<"unix:path=/tmp/dbus;;tcp:host=localhost,port=12345">>)
    ).

only_semicolon_test() ->
    ?assertMatch({error, _}, parse(<<";">>)).

%% The spec's unescaping rule -- "it is an error if a non-optionally-escaped
%% byte is seen unescaped" -- applies to `=' like any other byte outside
%% [-0-9A-Za-z_/.*]. Splitting the parameter on its FIRST `=' therefore
%% yields a value that is itself invalid, not a value containing a `='.
value_with_raw_equals_test() ->
    ?assertMatch(
        {error, _},
        parse(<<"unixexec:path=/usr/bin/example,argv1=a=b">>)
    ).

%% Likewise a raw `:' -- the address split takes the first one, and the rest
%% has to be escaped to survive.
value_with_raw_colon_test() ->
    ?assertMatch({error, _}, parse(<<"unix:path=/tmp/a:b">>)).

invalid_addresses() ->
    [
        <<"bad:address">>
    ].

bad_address_test_() ->
    [
        ?_assertMatch({error, _}, parse(Addr))
     || Addr <- invalid_addresses()
    ].
-endif.
