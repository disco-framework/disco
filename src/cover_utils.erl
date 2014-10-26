-module(cover_utils).
-export([init/0, analyze/0]).

-define(RESULTS_DIR, "integration-coverage/").

init() ->
    cover:start(),
    Results = cover:compile_beam_directory("ebin/"),
    Failures = [ Msg || {error, Msg} <- Results],
    io:format("Cover compile failures: ~p~n", [Failures]).

analyze() ->
    Modules = cover:modules(),

    NonTestModules = lists:filter(fun(Module) ->
                                          Es = lists:reverse(string:tokens(atom_to_list(Module),"_") ),
                                          lists:nth(1, Es)  =/= "tests"
                                  end, Modules),

    ok = filelib:ensure_dir(?RESULTS_DIR),

    _ = [cover:analyse_to_file(Module, cover_file_path(Module), [html]) || Module <- NonTestModules],

    RawResults = [cover:analyse(Module, coverage, module) || Module <- NonTestModules],

    PercentageResults = lists:sort(fun({_, PercA}, {_, PercB}) ->
                                           PercA =< PercB
                                   end,
                                   [{Module, Cov / (Cov + NotCov) * 100.0} || {ok, {Module, {Cov, NotCov}}} <- RawResults]),

    io:format("Cover analyze results:~n"),
    _ = [io:format("  " ++ string:left(atom_to_list(Module), 25) ++ " ~.1f%~n", [Percentage]) || {Module, Percentage} <- PercentageResults],

    {OverallCovered, OverallNotCovered} = lists:foldl(fun({ok, {_, {Cov, NotCov}}}, {AccCov, AccNotCov}) ->
                                                              {Cov + AccCov, NotCov + AccNotCov}
                                                      end, {0,0}, RawResults),

    OverallPercentage = OverallCovered / (OverallCovered + OverallNotCovered) * 100.0,

    io:format("~nCoverage details in " ++ ?RESULTS_DIR ++ "{Module}.COVER.html~n"),
    io:format("Overall code coverage through integration test:~n  ~.1f%~n~n", [OverallPercentage]).


cover_file_path(Module) ->
    ?RESULTS_DIR ++ atom_to_list(Module) ++ ".COVER.html".
