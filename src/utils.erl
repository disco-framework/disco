%% -------------------------------------------------------------------
%% @doc General utility functions for the DisCo Framework.
%% @end
%% -------------------------------------------------------------------

-module(utils).

-include("global_types.hrl").

-export([
         intersperse/2,
         join/2,
         wait/1,
         binary_to_atom/1,
         atom_to_binary/1,
         get_worker_reg_name/1,
         get_gui_reg_name/1,
         slice/2,
         pmap/2,
         get_argument/1
        ]).

%% @doc Inserts the separator between all elements of the list.
-spec intersperse(Separator :: A, List :: [A]) -> [A].
intersperse(_Separator, [])     -> [];
intersperse(_Separator, [X])    -> [X];
intersperse(Separator,  [X|Xs]) -> [ X | [ Separator | intersperse(Separator, Xs) ] ].

%% @doc Joins a list of strings into one, separated by the given separator.
-spec join(string(), [string()]) -> string().
join(Separator, List) -> lists:append(intersperse(Separator, List)).

%% @doc Lets the current process wait for some milliseconds.
-spec wait(non_neg_integer()) -> ok.
wait(Milliseconds) ->
    receive
    after Milliseconds -> ok
    end.

%% @doc Converts a binary into an utf8 encoded atom.
-spec binary_to_atom(binary()) -> atom().
binary_to_atom(Bin) ->
    binary_to_atom(Bin, utf8).

%% @doc Converts an atom into a binary, assuming utf8 encoding.
-spec atom_to_binary(atom()) -> binary().
atom_to_binary(Atom) ->
    atom_to_binary(Atom, utf8).

%% @doc Translates a worker ID into the name the interface process for that worker will be registered under.
-spec get_worker_reg_name(worker_id()) -> atom().
get_worker_reg_name(WorkerID) ->
    list_to_atom("worker_" ++ atom_to_list(WorkerID)).

%% @doc Translates a GUI index into the name the interface process for that gui will be registered under.
-spec get_gui_reg_name(integer()) -> atom().
get_gui_reg_name(GuiIdx) ->
    list_to_atom("gui_" ++ integer_to_list(GuiIdx)).

%% @doc Slices a list into a list of lists of the size N. The last sublist may be shorter.
-spec slice(pos_integer(), [any()]) -> [[any()]].
slice(N, List) ->
    slice(N, List, []).

%% @doc Parallel Map function.<br/>
%% Maps a function over a list, spawning a new process for each element.
%% Collects and returns the list of transformed values, order preserved.
-spec pmap(fun(), [any()]) -> [any()].
pmap(Fun, List) ->
    Parent = self(),
    [receive {Pid, Result} -> Result end
     || Pid <- [spawn(fun() -> Parent ! {self(), Fun(X)} end) || X <- List]].

%% @doc we need this proxy function for mocking
%% because meck can't mock the "init" module
-spec get_argument(atom()) -> error | {ok, term()}.
get_argument(Flag) ->
    init:get_argument(Flag).

%% ===================================================================
%% private functions
%% ===================================================================

-spec safe_split(pos_integer(), [any()]) -> {[any()],[any()]}.
safe_split(N, List) ->
    safe_split_(N, List, []).

-spec safe_split_(non_neg_integer(), [any()], [any()]) -> {[any()],[any()]}.
safe_split_(_, [], Acc) -> {lists:reverse(Acc), []};
safe_split_(0, List, Acc) -> {lists:reverse(Acc), List};
safe_split_(N, [Head|Rest], Acc) ->
    safe_split_(N-1, Rest, [Head|Acc]).

-spec slice(pos_integer(), [any()], [[any()]]) -> [[any()]].
slice(_, [], Acc) ->
    lists:reverse(Acc);
slice(N, List, Acc) ->
    {Part, Rest} = safe_split(N, List),
    slice(N, Rest, [Part|Acc]).
