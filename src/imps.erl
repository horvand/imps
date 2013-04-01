-module(imps).
-compile(export_all).
%% @headerfile "imps.hrl"
-include("imps.hrl").

-define(dbg(X),
        io:format("~s:~p: ~s = ~p\n", [?MODULE, ?LINE, ??X, X])).

-spec encode(Seq :: integer(), Direction :: request | response,
             Proto :: imps_proto(), Src :: itag(), Dst :: itag(), 
             Message :: imps_req_or_resp()) -> imps_packet().
encode(Seq, Direction, Proto, Src, Dst, Message) ->
    encode(#imps_packet{seq = Seq,
                        proto = Proto,
                        src = Src,
                        dst = Dst,
                        data = case Direction of
                                request -> Proto:encode_req(Message);
                                response -> Proto:encode_res(Message) end
                    }).
encode(#imps_packet{version = Version, seq = Seq,
                    proto = Proto,
                    src = Src, dst = Dst,
                    data = Data} = _PDU) ->
    Head = <<Version:32, Seq:32, (proto(Proto)):32, (_RESERVED = 0):32>>,
    Tail = <<(itag:from_integer(Src))/bitstring, (itag:from_integer(Dst))/bitstring,
             Data/binary>>,
    pad(insert_size(Head, Tail)).

print_packet(Binary) ->
    {Seq, Proto, Src, Dst, Data} = analyze_packet(Binary),
    io:format(standard_error,
        "IMPS packet #~w (~p)\n"
        "\tSource:\t\t~w\n"
        "\tDestination:\t~w\n\n"
        "~s\n\n",
        [Seq, Proto, Src, Dst,
         case Proto of
                chimp -> io_lib:format("~p", [Data]); %% FIXME
                _     -> chop_pad(Data) end]).


analyze_packet(<<1:32, Seq:32, NProto:32, 0:32, Rest/binary>> = _Packet) ->
    Proto = rproto(NProto),
    {_Size, R2}  = itag:get_itag(Rest),
    %Size = size(Packet), ???
    {Src, R3}   = itag:get_itag(R2),
    {Dst, Data} = itag:get_itag(R3),
    {Seq, Proto, Src, Dst, Data}.

chop_pad(Bits) when is_binary(Bits) -> binary_to_list(Bits);
chop_pad(Bits) ->
    [_RHead | RTail] = lists:reverse(erlang:bitstring_to_list(Bits)),
    lists:reverse(RTail).
proto(keeper)   -> 1;
proto(chimp)    -> 2;
proto(iamb_pent)-> 5;
proto(pan)      -> 10.

rproto(1)   -> keeper;
rproto(2)   -> chimp;
rproto(5)   -> iamb_pent;
rproto(10)  -> pan.

insert_size(H,T) -> insert_size(H,T,0).
insert_size(Head, Tail, N) ->
    S0 = byte_size(<<Head/binary, Tail/bitstring, 0:(N)>>),
    SI0 = itag:from_integer(S0),
    S1 = byte_size(<<Head/binary, SI0/bitstring, Tail/bitstring>>),
    SI1 = itag:from_integer(S1),
    case byte_size(SI1) - byte_size(SI0) of
       0 -> <<Head/binary, SI1/bitstring, Tail/bitstring>>;
       N -> insert_size(Head, Tail, N)
   end.

pad(Bits) ->
    Pad = case bit_size(Bits) rem 8 of 0 -> 0; N -> 8 - N end,
    <<Bits/bitstring, 0:(Pad)>>. %% <<0:0>> is legal, yeah!

print_bstr(Bits) ->
    io:format("~s\n",
              [[if is_integer(X) -> io_lib:format("~8.2.0B ", [X]);
                   is_bitstring(X) -> [integer_to_list(I) || <<I:1>> <= X] end
                || X <- bitstring_to_list(Bits)]]).

pad_test() ->
    <<>> = pad(<<>>),
    <<255>> = pad(<<255>>),
    <<255, 128>> = pad(<<255, 1:1>>),
    <<255,2>> = pad(<<255, 1:7>>).

test_p0() ->
    encode(666, request, iamb_pent, itag:uuid(), itag:uuid(),
                  {receiveth, "EvilMonkey.Test0"}).


test_test() ->
    print_packet(test_p0()).


