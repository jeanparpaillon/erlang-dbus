-module(prop_dbus_marshaller_wire).
-moduledoc """
Wire-level laws for `dbus_marshaller'.

Where `prop_dbus_marshaller' states how a value encodes, this module states how
a byte stream off a socket behaves:

1. a marshalled message decodes back to itself;
2. decoding is incremental -- splitting the stream anywhere and feeding it in
   two goes yields the same messages, in the same order, as one pass. A
   truncated message is `more', never an error, and never silently dropped;
3. decoding arbitrary and corrupted input terminates and returns one of the
   three documented answers, rather than raising. `unmarshal_data/1' is fed
   straight off the socket and the lengths it trusts are written by the peer.
""".

-include_lib("proper/include/proper.hrl").
-include_lib("dbus/include/dbus.hrl").

-export([
    prop_message_roundtrip/0,
    prop_incremental_decode/0,
    prop_never_crashes/0
]).

%%%
%%% 1. Messages
%%%

prop_message_roundtrip() ->
    ?FORALL(
        {Msg, Decoded},
        dbus_marshaller_gen:message(),
        begin
            Bin = iolist_to_binary(dbus_marshaller:marshal_message(Msg)),
            dbus_marshaller:unmarshal_data(Bin) =:= {ok, [Decoded], <<>>}
        end
    ).

%%%
%%% 2. Incremental decoding
%%%

prop_incremental_decode() ->
    ?FORALL(
        Pairs,
        dbus_marshaller_gen:messages(),
        begin
            Bin = marshal_all([M || {M, _} <- Pairs]),
            Expected = [D || {_, D} <- Pairs],
            ?FORALL(
                Cut,
                integer(0, byte_size(Bin)),
                begin
                    <<Prefix:Cut/binary, Suffix/binary>> = Bin,
                    case dbus_marshaller:unmarshal_data(Prefix) of
                        more ->
                            decodes_to(<<Prefix/binary, Suffix/binary>>, Expected);
                        {ok, Head, Rest} ->
                            Tail = lists:nthtail(length(Head), Expected),
                            Head =:= lists:sublist(Expected, length(Head)) andalso
                                decodes_to(<<Rest/binary, Suffix/binary>>, Tail);
                        {error, _} ->
                            false
                    end
                end
            )
        end
    ).

decodes_to(<<>>, Expected) ->
    %% An empty buffer is `more', not an empty result.
    Expected =:= [] andalso dbus_marshaller:unmarshal_data(<<>>) =:= more;
decodes_to(Bin, Expected) ->
    dbus_marshaller:unmarshal_data(Bin) =:= {ok, Expected, <<>>}.

%%%
%%% 3. Robustness
%%%

prop_never_crashes() ->
    ?FORALL(
        Bin,
        fuzz(),
        ?TIMEOUT(
            5000,
            case catch dbus_marshaller:unmarshal_data(Bin) of
                more -> true;
                {ok, Msgs, Rest} -> is_list(Msgs) andalso is_binary(Rest);
                {error, _} -> true;
                _Other -> false
            end
        )
    ).

fuzz() ->
    union([binary(), corrupted_stream()]).

%% A valid stream, corrupted: the shapes a real peer produces when it is buggy
%% or hostile, which random binaries almost never reach past the header check.
corrupted_stream() ->
    ?LET(
        Pairs,
        dbus_marshaller_gen:messages(),
        ?LET(
            Bin,
            exactly(marshal_all([M || {M, _} <- Pairs])),
            dbus_marshaller_gen:corrupted(Bin)
        )
    ).

marshal_all(Msgs) ->
    iolist_to_binary([dbus_marshaller:marshal_message(M) || M <- Msgs]).
