-module(gliua_rs).
-export([evaluate/1, value_test/1]).
-nifs([evaluate/1, value_test/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("priv/libgliua", 0).

evaluate(_stack) ->
    exit(nif_library_not_loaded).

value_test(_value) ->
    exit(nif_library_not_loaded).