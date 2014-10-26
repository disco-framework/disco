%% @hidden to edoc

-module(barkeeper_tests).
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


%%
%% tests

ask_test() ->
    MockedMods = [port_utils, json],
    meck:new(MockedMods, [passthrough]),
    meck:new(application, [passthrough, unstick]),

    meck:expect(port_utils, easy_open_killer_port, fun(_) -> foo_port end),
    PortCmdDoubleAnswer = fun(_,_) ->
                                  self() ! {foo_port, {data, {bar, <<"somebinstring">>}}},
                                  self() ! {foo_port, {data, {bar, <<"somebinstring">>}}}
                          end,
    PortCmdNoAnswer     = fun(_,_) -> ok end,
    meck:expect(application, get_env, fun(port_call_timeout) -> {ok, 10} end),
    meck:expect(json, default_decoder, fun() -> ok end),
    meck:sequence(json, process, 2, [{incomplete, foo},
                                     {ok, [{<<"worker input">>, [<<"AnswerString">>]}]},
                                     {bad_json, bar}]),

    {ok, SupPid} = barkeeper_sup:start_link("ignored executable path"),
    try
        meck:expect(port_utils, port_command, PortCmdDoubleAnswer),
        ?assertEqual({ok, ["AnswerString"]}, barkeeper:ask("ProblemString", "StateString")),
        ?assertEqual({error, illegal_json},  barkeeper:ask("ProblemString", "StateString")),
        meck:expect(port_utils, port_command, PortCmdNoAnswer),
        ?assertEqual({error, timeout},       barkeeper:ask("ProblemString", "StateString"))
    after
        exit(SupPid, normal),

        ?assert(meck:validate(MockedMods)),
        meck:unload(MockedMods),
        meck:unload(application)
    end.

-endif.
