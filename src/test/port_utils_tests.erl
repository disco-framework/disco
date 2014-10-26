%% @hidden to edoc

-module(port_utils_tests).
-ifdef(TEST).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


%% executes the propEr properties inside an eunit test
proper_test_() ->
    {timeout, 30,
     fun() ->
             ?assertEqual([], proper:module(port_utils, [{to_file, user}, long_result]))
     end}.

%%
%% tests

killer_port_test() ->
    meck:new(port_utils, [passthrough]),
    meck:new(application, [passthrough, unstick]),
    meck:expect(port_utils, spawn_port,
                fun(_, _) ->
                        ok
                end),
    meck:expect(application, get_env, fun(port_buffer_size) -> {ok, 42} end),

    try
        ?assertEqual(ok, port_utils:easy_open_killer_port("'ls'")),

        ?assert(meck:called(port_utils, spawn_port, ["bash -c '(cat && kill -9 0) | ('\\''ls'\\''; kill 0)'", [{line,42}, stream]])),
        ?assert(meck:validate(port_utils))
    after
        meck:unload(port_utils),
        meck:unload(application)
    end.

-endif.
