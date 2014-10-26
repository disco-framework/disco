%% @hidden to edoc

-module(json_tests).
-ifdef(TEST).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


%% executes the propEr properties inside an eunit test
proper_test_() ->
    {timeout, 30,
     fun() ->
             ?assertEqual([], proper:module(json, [{to_file, user}, long_result]))
     end}.

%%
%% tests

process_test() ->
    %% parse successful
    ?assertEqual({ok, [{}]}, json:process("{}", json:default_decoder())),

    %% incomplete json
    ?assertMatch({incomplete, {_, ["{"]}}, json:process("{", json:default_decoder())),

    %% invalid json
    ?assertMatch({bad_json, {_, []}}, json:process("{/}", json:default_decoder())).

handle_test() ->
    meck:new(fake, [non_strict]),

    meck:expect(fake, ok_handler,  fun(_DecodedObj) -> ok           end),
    meck:expect(fake, err_handler, fun(_DecodedObj) -> unknown_json end),

    %% handle successful
    ?assertEqual(ok, json:handle([{}], fun fake:ok_handler/1)),
    ?assert(meck:called(fake, ok_handler, [[{}]])),

    %% handle error
    ?assertMatch(unknown_json, json:handle([{}], fun fake:err_handler/1)),
    ?assert(meck:called(fake, err_handler, [[{}]])),

    ?assert(meck:validate(fake)),
    meck:unload(fake).

prop_to_json_msg() ->
    proper:check_spec({json, to_json_msg, 1}).

process_attrs_test() ->
    meck:new(fake, [non_strict]),

    meck:expect(fake, handler, fun(_) -> ok end),
    meck:expect(fake, never_called_handler, fun(_) -> throw(should_not_happen) end),

    %% with converter fun
    ?assertEqual(ok, json:process_attrs(fun fake:handler/1,
                                        [{<<"testval">>, fun(Val) -> Val+1 end}],
                                        [{<<"testval">>, 1}])),
    ?assert(meck:called(fake, handler, [2])),

    %% without converter fun
    ?assertEqual(ok, json:process_attrs(fun fake:handler/1,
                                        [<<"testval">>],
                                        [{<<"testval">>, 1}])),
    ?assert(meck:called(fake, handler, [1])),

    %% trying to access non-existent attribute
    ?assertEqual(unknown_json, json:process_attrs(fun fake:never_called_handler/1,
                                                  [{<<"not_there">>}],
                                                  [{<<"testval">>, 1}])),

    ?assert(meck:validate(fake)),
    meck:unload(fake).

-endif.
