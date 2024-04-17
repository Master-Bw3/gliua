-module(gliua_rs).
-export([empty_stack/0]).
-nifs([empty_stack/0]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("priv/libgliua", 0).

empty_stack() ->
    exit(nif_library_not_loaded).