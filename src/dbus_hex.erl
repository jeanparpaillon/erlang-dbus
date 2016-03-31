%% @copyright 2014 Jean Parpaillon
%% @author Jean Parpaillon <jean.parpaillon@free.fr>
%% @doc
%%
%% @end
-module(dbus_hex).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([encode/1,
	 decode/1]).

%%====================================================================
%% External functions
%%====================================================================

%% @doc Encode a binary string as hex
%% @end
-spec encode(binary()) -> binary().
encode(Bin) ->
    encode(Bin, <<>>).


%% @doc Decode an hex string
%% @end
-spec decode(binary()) -> binary().
decode(Bin) ->
    decode(Bin, <<>>).


%%%
%%% Priv
%%%
decode(<<>>, Acc) ->
    Acc;

decode(<< $\s, _/binary >>, Acc) ->
    Acc;

decode(<< $\r, _/binary >>, Acc) ->
    Acc;

decode(<< $\n, _/binary >>, Acc) ->
    Acc;

decode(<< H, L, Rest/binary >>, Acc) ->
    C = hex_to_int(H) * 16 + hex_to_int(L),
    decode(Rest, << Acc/binary, C >>).


hex_to_int(C) when C >= $0, C =< $9 ->
    C - $0;

hex_to_int(C) when C >= $a, C =< $f ->
    C - $a + 10.


encode(<<>>, Acc) ->
    Acc;

encode(<< C:8, Rest/binary  >>, Acc) ->
    {L, H} = int_to_hex(C),
    encode(Rest, << Acc/binary, L, H >>).


int_to_hex(N) when N < 256 ->
    { hex(N div 16), hex(N rem 16) }.


hex(N) when N < 10 ->
    $0 + N;

hex(N) when N >= 10, N < 16 ->
    $a + (N-10).
