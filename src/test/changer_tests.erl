%% @hidden to edoc

-module(changer_tests).
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


%%
%% tests

change_state_test() ->
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
                                     {ok, [{<<"state">>, <<"AnswerString">>}]},
                                     {bad_json, bar}]),

    {ok, SupPid} = changer_sup:start_link("ignored executable path"),
    try
        meck:expect(port_utils, port_command, PortCmdDoubleAnswer),
        ?assertEqual({ok, "AnswerString"},  changer:change_state("OldState", "Proposition")),
        ?assertEqual({error, illegal_json}, changer:change_state("OldState", "Proposition")),
        meck:expect(port_utils, port_command, PortCmdNoAnswer),
        ?assertEqual({error, timeout},      changer:change_state("OldState", "Proposition"))
    after
        exit(SupPid, normal),

        ?assert(meck:validate(MockedMods)),
        meck:unload(MockedMods),
        meck:unload(application)
    end.

-endif.
