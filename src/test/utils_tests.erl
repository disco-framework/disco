%% @hidden to edoc

-module(utils_tests).
-ifdef(TEST).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


%% executes the propEr properties inside an eunit test
proper_test_() ->
    {timeout, 30,
     fun() ->
             ?assertEqual([], proper:module(utils, [{to_file, user}, long_result]))
     end}.

%%
%% tests

prop_intersperse() ->
    proper:check_spec({utils, intersperse, 2}),
    ?FORALL({X,L}, {integer(),list(integer())},
            lists:member(X, utils:intersperse(X, L)) or (length(L) =< 1)).

join_test() ->
    ?assertEqual("Hallo, Bernd", utils:join(", ", ["Hallo", "Bernd"])).

wait_test() ->
    ?assertEqual(ok, utils:wait(0)).

conversions_test() ->
    ?assertEqual('foo', utils:binary_to_atom(<<"foo">>)),
    ?assertEqual(<<"bar">>, utils:atom_to_binary('bar')).

regnames_test() ->
    ?assertEqual(worker_bla, utils:get_worker_reg_name(bla)),
    ?assertEqual(gui_42, utils:get_gui_reg_name(42)).

slice_test() ->
    ?assertEqual([[1,2,3],[4,5,6]], utils:slice(3, [1,2,3,4,5,6])),
    ?assertEqual([[1,2,3],[4,5]], utils:slice(3, [1,2,3,4,5])),
    ?assertEqual([], utils:slice(3, [])).

pmap_test() ->
    Fun = fun(E) -> E+1 end,
    List = [0,1,2,3,4,5,6,7,8,9],
    ?assertEqual(lists:map(Fun, List), utils:pmap(Fun, List)).

-endif.
