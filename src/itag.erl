-module(itag).
-export([from_integer/1,
         to_integer/1,
         get_itag/1]).

-define(DEBUG, 1).

-include("imps.hrl").

-ifdef(DEBUG).
-compile(export_all).
-define(dbg(X),
        io:format("~s:~p: ~s = ~p\n", [?MODULE, ?LINE, ??X, X])).
-endif.

-spec from_integer(integer()) -> itag().
from_integer(Int) ->
    Bytes = binary:encode_unsigned(Int),
    Size = size(Bytes),
    SizeBits = integer_to_bitstring(Size),
    SizeSize = bit_size(SizeBits),
    <<(ones(SizeSize))/bitstring, 0:1, SizeBits/bitstring, 
      Bytes/binary>>.

-spec to_integer(itag()) -> integer().
to_integer(ITag) ->
    {SizeSize, SizeData} = get_sizesize(ITag),
    <<Size:(SizeSize), Data/binary>> = SizeData,
    Size = size(Data), %% FIXME badarg
    binary:decode_unsigned(Data).

-spec get_itag(bitstring()) -> {ITag :: integer(), Rest :: bitstring()}.
get_itag(Bin) ->
    {SizeSize, Bin1} = get_sizesize(Bin),
    <<Size:(SizeSize), Bin2/bitstring>> = Bin1,
    <<ITag:(Size)/binary, Rest/bitstring>> = Bin2,
    {ITag, Rest}.


-spec uuid() -> integer().
uuid() ->
    list_to_integer(
        [X || X <- os:cmd("uuidgen"),
              lists:member(X, "0123456789ABCDEF")],
        16).


-spec bitstring_to_integer(bitstring()) -> integer().
bitstring_to_integer(Bits) ->
    PadLen = case bit_size(Bits) rem 8 of 0 -> 0;
                                          N -> 8 - N end,
    binary:decode_unsigned(<<0:(PadLen), Bits/bitstring>>).

-spec integer_to_bitstring(integer()) -> bitstring().
integer_to_bitstring(I) ->
    drop_zeroes(binary:encode_unsigned(I)).
 

%% 0 or 1 as length is encoded on 1 bit

-spec drop_zeroes(bitstring()) -> bitstring().
drop_zeroes(<<Bit:1>>)                          -> <<Bit:1>>; 
drop_zeroes(<<1:1, _/bitstring>> = BitString)  -> BitString;
drop_zeroes(<<0:1, Rest/bitstring>>)           -> drop_zeroes(Rest).


-spec get_sizesize(binary()) -> {integer(), bitstring()}.
get_sizesize(Bin)                        -> get_sizesize(Bin, _S = 0).
get_sizesize(<<1:1, Rest/bitstring>>, S) -> get_sizesize(Rest, S + 1);
get_sizesize(<<0:1, Rest/bitstring>>, S) -> {S, Rest}.

-spec ones(integer()) -> bitstring().
ones(N) -> << <<1:1>> || _ <- lists:seq(1, N)>>.

-ifdef(DEBUG).
print_bstr(Bits) ->
    io:format("~s\n",
              [[if is_integer(X) -> io_lib:format("~8.2.0B ", [X]);
                   is_bitstring(X) -> [integer_to_list(I) || <<I:1>> <= X] end
                || X <- bitstring_to_list(Bits)]]).
-endif.
