-module(pw_app).

-behavior(application).

-export([start/2,
         stop/1]).

-export([solve/4,
         printer/0]).

-define(PROVIDERS, [provider_a, provider_b]).

start(_Type, _Args) ->
    Numbers = parse_line(io:get_line([])),
    Target = parse_line(io:get_line([])),
    PrinterPid = spawn(?MODULE, printer, []),
    register(printer, PrinterPid),
    debug("finished parsing"),
    _ = [subs:spawn_provider(Name, Numbers, 1) || Name <- ?PROVIDERS],
    NumSchedulers = erlang:system_info(schedulers_online),
    start_solvers(NumSchedulers div 2 + 1, Target, provider_a, with_outer_div),
    start_solvers(NumSchedulers div 2 - 1, Target, provider_b, without_outer_div),
    receive
        nothing_whatsoever -> ok
    end,
    {ok, self()}.

stop(_State) ->
    ok.

-spec debug(string()) -> ok.
debug(S) ->
    printer ! {debug, self(), S},
    ok.
-spec debug(string(), [term()]) -> ok.
debug(Format, Args) ->
    printer ! {debug, self(), Format, Args},
    ok.

-spec start_solvers(non_neg_integer(), pos_integer(), atom(), constraint()) -> ok.
start_solvers(0, _, _, _) ->
    ok;
start_solvers(Num, Target, Provider, Constraint) ->
    spawn(?MODULE, solve, [Target, Provider, Constraint, []]),
    start_solvers(Num-1, Target, Provider, Constraint).

-spec parse_line(string()) -> term().
parse_line(Line) ->
    TermString = lists:sublist(Line, length(Line)-1) ++ ".",
    {ok, Tokens, _Rest} = erl_scan:string(TermString),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

-type operator() :: '+' | '-' | '*' | '/' | '<+>' | '<->' | '<*>'.
-type expr() :: {val, pos_integer()} | {app, operator(), expr(), expr()}.
-type val_comp() :: {Value :: pos_integer(), Complexity :: pos_integer()}.
-type result() :: {expr(), val_comp()}.
-type constraint() :: with_outer_div | without_outer_div | none.

-ifdef(modulo).
 -define(OPS, ['<+>', '/', '<*>', '+', '<->', '*', '-', '%']).
-else.
 -define(OPS, ['<+>', '/', '<*>', '+', '<->', '*', '-']).
-endif.
%-define(OPS, ['<+>', '/', '<*>', '+', '<->']).

-spec valid(operator(), pos_integer(), pos_integer()) -> boolean().
valid('+',   A, B) -> A >= B;
valid('-',   A, B) -> A > B;
valid('*',   A, B) -> A >= B;
valid('/',   A, B) -> A rem B == 0;
valid('<+>', A, B) -> A >= B;
valid('<->', A, B) -> A > B;
valid('<*>', A, B) -> A > B;
valid('%',   A, B) -> A rem B /= 0.

-spec appl(operator(), pos_integer(), pos_integer()) -> pos_integer().
appl('+',   A, B) -> A+B;
appl('-',   A, B) -> A-B;
appl('*',   A, B) -> A*B;
appl('/',   A, B) -> A div B;
appl('<+>', A, B) -> C = A+B, C*C;
appl('<->', A, B) -> C = A-B, C*C;
appl('<*>', A, B) -> (A+B)*(A-B);
appl('%',   A, B) -> A rem B.

-spec interleave(A, [A]) -> [[A]].
interleave(X, [])      -> [[X]];
interleave(X, [X|T]) -> lists:map(fun(L) -> [X|L] end, interleave(X, T));
interleave(X, Y=[H|T]) -> [[X|Y]|lists:map(fun(L) -> [H|L] end, interleave(X, T))].

%%% TODO do not permute same numbers ([1,1,1,1,1,1,1,1,1])
%%% beziehungsweise: keine doppelten permutationen erzeugen, bitte.
%%% evtl auch für subs interessant
-spec perms([A]) -> [[A]].
perms(L) ->
    uniq(perms_(L)).
perms_([])    -> [[]];
perms_([H|T]) -> lists:flatmap(fun(Perm) -> interleave(H, Perm) end, perms_(T)).

-spec choices([[A]]) -> [[A]].
choices(Subs) -> lists:flatmap(fun perms/1, Subs).

-spec split([A]) -> [{[A], [A]}].
split([])    -> [];
split([_])   -> [];
split([H|T]) -> [{[H], T}|[{[H|Ls], Rs} || {Ls, Rs} <- split(T)]].

%%% TODO: try depth-first
-spec results([pos_integer()]) -> [result()].
results([])  -> [];
results([N]) -> [{{val, N}, {N, N}}];
results(Ns)  ->
      [Res || {Ls, Rs} <- split(Ns),
              LX <- results(Ls),
              RY <- results(Rs),
              Res <- combine_(LX, RY, ?OPS)].

-spec results_wo_outer_div([pos_integer()]) -> [result()].
results_wo_outer_div([])  -> [];
results_wo_outer_div([N]) -> [{{val, N}, {N, N}}];
results_wo_outer_div(Ns)  ->
    [Res || {Ls, Rs} <- split(Ns),
            LX <- results(Ls),
            RY <- results(Rs),
            Res <- combine_(LX, RY, ?OPS -- ['/'])].

-spec results_w_outer_div([pos_integer()]) -> [result()].
results_w_outer_div([])  -> [];
results_w_outer_div([N]) -> [{{val, N}, {N, N}}];
results_w_outer_div(Ns)  ->
    [Res || {Ls, Rs} <- split(Ns),
            LX <- results(Ls),
            RY <- results(Rs),
            Res <- combine_(LX, RY, ['/'])].

-spec combine_(result(), result(), [operator()]) -> [result()].
combine_({L,X={XVal, _}},{R,Y={YVal, _}}, Operators) ->
    [{{app, Op, L, R}, apply_comp(Op, X, Y)} ||
        Op <- Operators,
        valid(Op, XVal, YVal)].

-spec apply_comp(operator(), val_comp(), val_comp()) -> val_comp().
apply_comp(Op, {XVal, XComp}, {YVal, YComp}) ->
    Val = appl(Op, XVal, YVal),
    {Val, Val + XComp + YComp}.

%% TODO: try spawning a process for second half of choices
-spec gen_solutions(pos_integer(), constraint(), [[pos_integer()]]) -> ok.
gen_solutions(Target, Constraint, Subs) ->
    Self = self(),
    ResultFun = case Constraint of
                    none -> fun results/1;
                    without_outer_div -> fun results_wo_outer_div/1;
                    with_outer_div -> fun results_w_outer_div/1
                end,
    lists:foreach(fun(Choice) ->
                      Results = [{Expr, Comp} || {Expr, {Val, Comp}} <- ResultFun(Choice),
                                                 Val == Target],
                      lists:foreach(fun(S) -> printer ! {print, Self, S} end, Results)
              end, choices(Subs)),
    ok.

-spec other_constraint(constraint()) -> constraint().
other_constraint(with_outer_div) -> without_outer_div;
other_constraint(without_outer_div) -> with_outer_div;
other_constraint(none) -> none.

-spec solve(pos_integer(), atom(), constraint(), [atom()]) -> ok.
solve(Target, Provider, Constraint, EmptyProviders) ->
    case subs:get_package(Provider) of
        [] -> % provider empty
            %% try to switch to another provider
            NewEmptyProviders = EmptyProviders ++ [Provider],
            case ?PROVIDERS -- NewEmptyProviders of
                [NewProvider|_] ->
                    solve(Target, NewProvider, other_constraint(Constraint), NewEmptyProviders);
                _ ->
                    %% just block, so the printer process has time to finish
                    receive
                        none -> ok
                    end
            end;
        SubPackage ->
            gen_solutions(Target, Constraint, SubPackage),
            solve(Target, Provider, Constraint, EmptyProviders)
    end.

-spec expr_to_s(expr()) -> string().
expr_to_s({val, N}) -> integer_to_list(N);
expr_to_s({app, Op, A, B}) ->
    "(" ++ expr_to_s(A) ++ " " ++ atom_to_list(Op) ++ " " ++ expr_to_s(B) ++ ")".

print_prefix(StartTime, Process, Comp) ->
    Age = timer:now_diff(now(), StartTime) / 1000 / 1000,
    io:format(standard_error, "~n[~7.3fs, ~p, ~12B]  ", [Age, Process, Comp]).

printer() ->
  process_flag(priority, high),
  printer_loop(now(), 0).

-spec printer_loop(erlang:timestamp(), non_neg_integer()) -> no_return().
printer_loop(StartTime, Best) ->
    receive
        {print, Asker, {Expr, Comp}} when Comp > Best ->
            print_prefix(StartTime, Asker, Comp),
            io:format("~s~n", [expr_to_s(Expr)]),
            printer_loop(StartTime, Comp);
        {print, _, _} ->
            printer_loop(StartTime, Best);
        {debug, Asker, String} ->
            print_prefix(StartTime, Asker, 0),
            io:format(standard_error, String ++ "~n", []),
            printer_loop(StartTime, Best);
        {debug, Asker, Format, Args} ->
            print_prefix(StartTime, Asker, 0),
            io:format(standard_error, Format ++ "~n", Args),
            printer_loop(StartTime, Best);
        Other ->
            io:format(standard_error, "Printer does not understand ~p~n", [Other]),
            printer_loop(StartTime, Best)
    end.

-spec uniq([A]) -> [A].
uniq(List) ->
    lists:usort(List).
