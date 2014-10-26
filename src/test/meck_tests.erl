%% @hidden to edoc

-module(meck_tests).
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


%%
%% tests

meck_test() ->
    meck:new(fake, [non_strict]), % mocked module doesn't have to exist
    meck:expect(fake, fake, fun(_) -> ok end),

    ?assertEqual([], meck:history(fake)),
    ?assert(meck:validate(fake)), % only checks for _wrong_ calls to expected functions, not for call existence

    fake:fake(bla),

    ?assert(meck:called(fake, fake, [bla])), % checking for calls
    ?assert(meck:validate(fake)),

    meck:unload(fake).

-endif.
