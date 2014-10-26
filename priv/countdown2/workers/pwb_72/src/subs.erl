-module(subs).

-export([spawn_provider/3,
         get_package/1]).

%%% internal process fun
-export([provider/4]).

-spec spawn_provider(atom(), [pos_integer()], pos_integer()) -> pid().
spawn_provider(Name, Numbers, ChunkSize) ->
    Pid = spawn(?MODULE, provider, [Name, Numbers, ChunkSize, self()]),
    receive
        {providing, Pid} -> Pid
    end.

-spec get_package(atom()) -> [[pos_integer()]].
get_package(Provider) ->
    Ref = make_ref(),
    Provider ! {get_package, self(), Ref},
    receive
        {package, Ref, Package} ->
            Package
    %% after
    %%     100 ->
    %%         io:format(standard_error, "get sub package timeout ~n", [])
    end.


%%% private

-spec provider(atom(), [pos_integer()], pos_integer(), pid()) -> no_return().
provider(Name, Numbers, ChunkSize, Spawner) ->
    register(Name, self()),
    Spawner ! {providing, self()},
    _ = random:seed(random:seed0()),
    MagicNumber = 7,
    {RawSmallSubs, RawLargeSubs} = lists:splitwith(
                                     fun(Sub) ->
                                             length(lists:usort(Sub)) < MagicNumber
                                     end,
                                     subs(Numbers)),
    SmallSubs = shuffle(lists:usort(RawSmallSubs)),
    LargeSubs = lists:sort(fun(A,B) ->
                                   length(A) =< length(B)
                           end,
                           lists:usort(RawLargeSubs)),
    % Really large subs should appear at the end
    Subs = SmallSubs ++ LargeSubs,
    io:format(standard_error, "Provider ~p starting with ~p subs~n", [Name, length(Subs)]),
    provider_loop(Name, Subs, ChunkSize).

-spec provider_loop(atom(), [[pos_integer()]], pos_integer()) -> no_return().
provider_loop(Name, Subs, ChunkSize) ->
    receive
        {get_package, Asker, Ref} ->
            SubsLeft = length(Subs),
            {Package, Rest} =
                if
                    SubsLeft > ChunkSize ->
                        lists:split(ChunkSize, Subs);
                    true ->
                        {Subs, []}
                end,
%            io:format(standard_error, ".~p:~p", [Name, SubsLeft]),
%            io:format(standard_error, ".", []),
            Asker ! {package, Ref, Package},
            provider_loop(Name, Rest, ChunkSize);
        Other ->
            io:format(standard_error, "Sub Provider does not understand ~p~n", [Other]),
            provider_loop(Name, Subs, ChunkSize)
    end.



-spec subs([A]) -> [[A]].
subs([]) -> [[]];
subs([H|T]) -> Rest = subs(T),
               Rest ++ lists:map(fun(L) -> [H|L] end, Rest).

shuffle(List) ->
    %% Determine the log n portion then randomize the list.
    randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
    randomize(List);
randomize(T, List) ->
    lists:foldl(fun(_E, Acc) ->
                        randomize(Acc)
                end, randomize(List), lists:seq(1, (T - 1))).

randomize(List) ->
    D = lists:map(fun(A) ->
                          {random:uniform(), A}
                  end, List),

    {_, D1} = lists:unzip(lists:keysort(1, D)),
    D1.
