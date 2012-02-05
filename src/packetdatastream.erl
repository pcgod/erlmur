-module(packetdatastream).

-export([new/1, encode_int/1, decode_int/1]).

-record(pds, {}).

new(data) ->
	ok.


-spec encode_int(I :: integer()) -> binary().

encode_int(I) when I =< -16#04 ->
	A = encode_int(-I),
	<<1:1, 1:1, 1:1, 1:1, 1:1, 0:1, 0:2, A/binary>>;
encode_int(I) when I >= -16#03 andalso I =< -16#01 ->
	<<1:1, 1:1, 1:1, 1:1, 1:1, 1:1, (-I):2>>;
encode_int(I) when I =< 16#7f ->
	<<0:1, I:7>>;
encode_int(I) when I =< 16#3fff ->
	A = I bsr 8,
	Rest = I band 16#ff,
	<<1:1, 0:1, A:6, Rest:8>>;
encode_int(I) when I =< 16#1fffff ->
	A = I bsr 16,
	B = (I bsr 8) band 16#ff,
	C = I band 16#ff,
	<<1:1, 1:1, 0:1, A:5, B:8, C:8>>;
encode_int(I) when I =< 16#fffffff ->
	A = I bsr 24,
	B = (I bsr 16) band 16#ff,
	C = (I bsr 8) band 16#ff,
	D = I band 16#ff,
	<<1:1, 1:1, 1:1, 0:1, A:4, B:8, C:8, D:8>>;
encode_int(I) when I =< 16#ffffffff ->
	A = I bsr 24,
	B = (I bsr 16) band 16#ff,
	C = (I bsr 8) band 16#ff,
	D = I band 16#ff,
	<<1:1, 1:1, 1:1, 1:1, 0:1, 0:1, A:2, B:8, C:8, D:8>>;
encode_int(I) when I =< 16#ffffffffffffffff ->
	A = I bsr 56,
	B = (I bsr 48) band 16#ff,
	C = (I bsr 40) band 16#ff,
	D = (I bsr 32) band 16#ff,
	E = (I bsr 24) band 16#ff,
	F = (I bsr 16) band 16#ff,
	G = (I bsr 8) band 16#ff,
	H = I band 16#ff,
	<<1:1, 1:1, 1:1, 1:1, 0:1, 1:1, A:2, B:8, C:8, D:8, E:8, F:8, G:8, H:8>>;
encode_int(I) ->
	erlang:error(badarg, [I]).


-spec decode_int(Bytes :: binary()) -> {integer(), binary()}.

decode_int(<<0:1, A:7, Rest/binary>>) ->
	{A, Rest};
decode_int(<<1:1, 0:1, A:6, B:8, Rest/binary>>) ->
	{(A bsl 8) bor B, Rest};
decode_int(<<1:1, 1:1, 0:1, A:5, B:8, C:8, Rest/binary>>) ->
	{(A bsl 16) bor (B bsl 8) bor C, Rest};
decode_int(<<1:1, 1:1, 1:1, 0:1, A:4, B:8, C:8, D:8, Rest/binary>>) ->
	{(A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D, Rest};
decode_int(<<1:1, 1:1, 1:1, 1:1, 0:1, 0:1, A:2, B:8, C:8, D:8, Rest/binary>>) ->
	{(A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D, Rest};
decode_int(<<1:1, 1:1, 1:1, 1:1, 0:1, 1:1, A:2, B:8, C:8, D:8, E:8, F:8, G:8, H:8, Rest/binary>>) ->
	{(A bsl 56) bor (B bsl 48) bor (C bsl 40) bor (D bsl 32) bor (E bsl 24) bor (F bsl 16) bor (G bsl 8) bor H, Rest};
decode_int(<<1:1, 1:1, 1:1, 1:1, 1:1, 0:1, _A:2, Rest/binary>>) ->
	{X, NewRest} = decode_int(Rest),
	{- X, NewRest};
decode_int(<<1:1, 1:1, 1:1, 1:1, 1:1, 1:1, A:2, Rest/binary>>) ->
	{- A, Rest};
decode_int(Bytes) ->
	erlang:error(badarg, [Bytes]).
