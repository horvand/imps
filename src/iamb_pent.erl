-module(iamb_pent).
-include("imps.hrl").
-compile(export_all).

aborteth_default_msg() -> ["Fate", "may", "one", "day", "bless", "my", "zone"].
hark_default_msg() -> ["now,", "what", "light", "through", "yonder", "window", "breaks?"].
prithee_default_msg() -> ["thy", "monkey's", "wisdom", "poureth", "forth!"].
regretteth_default_msg() -> ["none", "hath", "writ", "thy", "words", "before"].

encode_req(aborteth) -> encode_req({aborteth, aborteth_default_msg()});
encode_req({receiveth, Name}) ->
    imps_util:encode_line(["RECEIVETH ", Name]);
encode_req({anon, Data}) ->
    imps_util:encode_line(["ANON ", Data]);
encode_req({aborteth, _Syls} = Req) ->
    with_syls(Req).

encode_res(hark) -> encode_res({hark, hark_default_msg()});
encode_res(prithee) -> encode_res({prithee, prithee_default_msg()});
encode_res(regretteth) -> encode_res({regretteth, regretteth_default_msg()});
encode_res(accepteth) -> erlang:error(
        tobb_dolgok_vannak_egen_s_foldon_horatio_mintsem_bolcselmetek_elgondolni_kepes);
encode_res({accepteth, _}) -> erlang:error(
        tobb_dolgok_vannak_egen_s_foldon_horatio_mintsem_bolcselmetek_elgondolni_kepes);
encode_res({_Atom, _Syls} = Req) -> with_syls(Req).

with_syls({Atom, Syls}) ->
    imps_util:encode_line(string:join([imps_util:upstr(Atom) | Syls], " ")).

