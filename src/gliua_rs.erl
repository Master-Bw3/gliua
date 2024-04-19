-module(gliua_rs).
-export([new_runtime/0, evaluate/1, to_string/1]).
-nifs([new_runtime/0, evaluate/1, to_string/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("priv/libgliua", 0).

evaluate(_stack) ->
    exit(nif_library_not_loaded).

new_runtime() ->
    exit(nif_library_not_loaded).

to_string(_value) ->
    exit(nif_library_not_loaded).
