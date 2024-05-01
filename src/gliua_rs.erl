-module(gliua_rs).
-export([new_runtime/0, evaluate/1, stack/1, to_string/1, rows/1, as_int/1, as_float/1, as_bool/1, as_char/1, as_complex/1]).
-nifs([new_runtime/0, evaluate/1, stack/1, to_string/1, rows/1, as_int/1, as_float/1, as_bool/1, as_char/1, as_complex/1]).
-on_load(init/0).

init() ->
    Dir = code:lib_dir(gliua, priv),
    ok = erlang:load_nif(Dir ++ "/libgliua", 0).

evaluate(_stack) ->
    exit(nif_library_not_loaded).

stack(_runtime) ->
    exit(nif_library_not_loaded).

new_runtime() ->
    exit(nif_library_not_loaded).

to_string(_value) ->
    exit(nif_library_not_loaded).

rows(_value) ->
    exit(nif_library_not_loaded).

as_int(_value) ->
    exit(nif_library_not_loaded).

as_float(_value) ->
    exit(nif_library_not_loaded).

as_bool(_value) ->
    exit(nif_library_not_loaded).

as_complex(_value) ->
    exit(nif_library_not_loaded).

as_char(_value) ->
    exit(nif_library_not_loaded).