-module(gliua_rs).
-export([evaluate/1, to_string/1, uncouple/1, join/2, couple/2]).
-nifs([evaluate/1, to_string/1, uncouple/1, join/2, couple/2]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("priv/libgliua", 0).

evaluate(_stack) ->
    exit(nif_library_not_loaded).

to_string(_value) ->
    exit(nif_library_not_loaded).

uncouple(_value) ->
    exit(nif_library_not_loaded).

join(_value, _value2) ->
    exit(nif_library_not_loaded).

couple(_value, _value2) ->
    exit(nif_library_not_loaded).