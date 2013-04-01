-module(imps_util).
-compile(export_all).

line_end() -> "\r\n".

encode_line(IOList) ->
    iolist_to_binary([IOList, line_end()]).


upstr(Atom) -> string:to_upper(atom_to_list(Atom)). %% FIXME not the safest way :(
