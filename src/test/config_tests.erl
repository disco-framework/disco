%% @hidden to edoc

-module(config_tests).
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%%
%% tests

check_test() ->
    %% mock your heart out.
    MockedMods = [utils, json],
    meck:new(MockedMods, [passthrough]),
    meck:new(application, [passthrough, unstick]),

    OkFun = fun(undefined) -> undefined;
               (Val)       -> {ok, Val}
            end,

    ValidAppEnv = fun(worker_run_cmd)          -> "somedir";
                     (worker_name_cmd)         -> "somedir";
                     (workers)                 -> [{a_worker, '42.42.42.42', "some_group"}];
                     (problems)                -> [{"Some Problem", "1+1 = ?", 10, "[1,1,1,2,3,4]"}];
                     (gui)                     -> [""];
                     (autosave_dir)            -> "somedir";
                     (cluster_start_mode)      -> sequential;
                     (validator)               -> "somepath";
                     (barkeeper)               -> "somepath";
                     (changer)                 -> "somepath";
                     (remote_node_path_prefix) -> "somedir";
                     (worker_call_timeout)     -> 1;
                     (port_call_timeout)       -> 1;
                     (port_buffer_size)        -> 1;
                     (startup_slice_size)      -> 1;
                     (do_component_test)       -> true;
                     (component_test_timeout)  -> 1;
                     (score_mode)              -> ranked
                  end,

    InvalidAppEnv1 = fun(validator)               -> invalidformatpath;
                        (barkeeper)               -> "somepath";
                        (changer)                 -> "somepath";
                        (remote_node_path_prefix) -> "somedir";
                        (port_call_timeout)       -> invalid_timeout;
                        (port_buffer_size)        -> -1;
                        (autosave_dir)            -> disabled;
                        (startup_slice_size)      -> 1;
                        (component_test_timeout)  -> 1;
                        (_)                       -> undefined
                     end,

    Errors1 = [
               "Workers run command undefined",
               "Workers name command undefined",
               "Worker list undefined",
               "Problem list undefined",
               "Validator command is not a string: invalidformatpath",
               "GUI command list undefined",
               "Worker call timeout undefined",
               "Port call timeout has invalid format: invalid_timeout",
               "Port buffer size out of range: -1",
               "Cluster start mode undefined",
               "Score mode undefined",
               "Component test flag undefined"
              ],

    InvalidAppEnv2 = fun(workers)            -> [];
                        (problems)           -> [];
                        (gui)                -> invalid_format;
                        (cluster_start_mode) -> invalid_format;
                        (Other)              -> ValidAppEnv(Other)
                     end,

    Errors2 = [
               "Worker list is empty",
               "Problem list is empty",
               "GUI command list has invalid format: invalid_format",
               "Cluster start mode has invalid format: invalid_format"
              ],

    InvalidAppEnv3 = fun(workers) -> [{a_worker, '42.42.42.42', "some_group"},
                                      {a_worker, '42.42.42.42', "some_group"}];
                        (Other)   -> ValidAppEnv(Other)
                     end,

    Errors3 = [
               "Worker list contains duplicate IDs"
              ],

    try
        meck:expect(application, get_env, fun(Key) -> OkFun(ValidAppEnv(Key)) end),
        ?assertEqual([], config:check_verbose()),
        ?assertEqual(ok, config:check()),

        meck:expect(application, get_env, fun(Key) -> OkFun(InvalidAppEnv1(Key)) end),
        GotErrors1 = config:check_verbose(),
        ?assertEqual([], lists:subtract(GotErrors1, Errors1)),
        ?assertEqual([], lists:subtract(Errors1, GotErrors1)),
        ?assertEqual(error, config:check()),

        meck:expect(application, get_env, fun(Key) -> OkFun(InvalidAppEnv2(Key)) end),
        GotErrors2 = config:check_verbose(),
        ?assertEqual([], lists:subtract(GotErrors2, Errors2)),
        ?assertEqual([], lists:subtract(Errors2, GotErrors2)),
        ?assertEqual(error, config:check()),

        meck:expect(application, get_env, fun(Key) -> OkFun(InvalidAppEnv3(Key)) end),
        GotErrors3 = config:check_verbose(),
        ?assertEqual([], lists:subtract(GotErrors3, Errors3)),
        ?assertEqual([], lists:subtract(Errors3, GotErrors3)),
        ?assertEqual(error, config:check())
    after
        ?assert(meck:validate(MockedMods)),
        meck:unload(MockedMods),
        meck:unload(application)
    end.

-endif.
