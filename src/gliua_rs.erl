-module(gliua_rs).
-export([empty_stack/0, push/2, add/1, take_stack/1, push_op/2]).
-nifs([empty_stack/0, push/2, add/1, take_stack/1, push_op/2]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("priv/libgliua", 0).

empty_stack() ->
    exit(nif_library_not_loaded).

push(_stack, _num) ->
    exit(nif_library_not_loaded).

add(_stack) ->
    exit(nif_library_not_loaded).

take_stack(_stack) ->
    exit(nif_library_not_loaded).

push_op(_op, _stack) ->
    exit(nif_library_not_loaded).