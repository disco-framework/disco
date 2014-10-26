%% @hidden to edoc

-module(test_helpers).

-export([
         unconsult/2,
         calls_of/2,
         times_called/2
        ]).

-spec unconsult(file:name(), [term()]) -> ok.
unconsult(File,Terms) ->
        {ok, Handle} = file:open(File, [write]),
        lists:foreach( fun(X) -> io:format(Handle, "~p.~n",[X]) end, Terms),
        file:close(Handle).

-spec calls_of(atom(), atom()) -> list(term()).
calls_of(Mod, FunName) ->
    FilterFunc = fun (CallSpec) ->
                         case CallSpec of
                             {_, {Mod, FunName, _}, _} -> true;
                             _                         -> false
                         end
                 end,
    lists:filter(FilterFunc, meck:history(Mod)).

-spec times_called(atom(), atom()) -> non_neg_integer().
times_called(Mod, FunName) ->
    length(calls_of(Mod, FunName)).
