-module(chimp).
-include("imps.hrl").
-compile(export_all).

-spec encode_req(chimp_req()) -> binary().
encode_req(bye) ->
    imps_util:encode_line("BYE");
encode_req({transcript, Data}) ->
    iolist_to_binary([
            imps_util:encode_line(["TRANSCRIPT ", integer_to_list(size(Data))]),
            Data]);
encode_req({Action, Object}) ->
    imps_util:encode_line([imps_util:upstr(Action), " ",imps_util:upstr(Object)]).


-spec encode_resp(chimp_resp()) -> binary().
encode_resp({helo, Text}) ->
    imps_util:encode_line(["HELO ", Text]);
encode_resp(Other) ->
    imps_util:encode_line(imps_util:upstr(Other)).

