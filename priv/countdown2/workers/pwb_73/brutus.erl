-module(brutus).
-author('Florian Grabbe').
-export([start/0, attack/2]).

start() -> Numbers = read_and_eval_line(),
           Result  = read_and_eval_line(),
           NumsOk = is_list(Numbers) andalso lists:all(fun erlang:is_integer/1, Numbers),
           ResOk  = is_integer(Result),
           if not NumsOk -> print_error("List of integer values in the first line expected!");
              not ResOk  -> print_error("Integer value in the second line expected!");
              true       -> print_debug("Numbers = ~p", [Numbers]),
                            print_debug("Result  = ~p", [Result]),
                            print_macro_info(),
                            attack(Numbers, Result)
           end,
           init:stop().


read_and_eval_line() -> catch begin {ok, Tokens, _}  = erl_scan:string(io:get_line("") ++ "."),
                                    {ok, Exprs}      = erl_parse:parse_exprs(Tokens),
                                    {value, Expr, _} = erl_eval:exprs(Exprs, []),
                                    Expr
                              end.


print_error(Msg)          -> io:format(standard_error, "Error: " ++ Msg ++ "~n", []).
print_debug(Format, Data) -> io:format(standard_error, Format ++ "~n", Data).


-spec uniq_slices( [ integer() ] ) -> [ { integer(), [ integer() ] } ].
%% uniq_slices([1,2,3])     => [{1,[2,3]}, {2,[1,3]}, {3,[1,2]}]
%% uniq_slices([1,1,3])     => [{1,[1,3]}, {3,[1,1]}]
%% uniq_slices([4,3,1,5,3]) => [{1,[4,3,5,3]}, {3,[4,1,5,3]}, {4,[3,1,5,3]}, {5,[4,3,1,3]}]
uniq_slices(Numbers) -> Slices = [ begin {L1, [X|L2]} = lists:split(N, Numbers),
                                         {X, L1 ++ L2}
                                   end
                                   || N <- lists:seq(0, length(Numbers) - 1) ],
                        lists:usort(fun ({X, _}, {Y, _}) -> X =< Y end, Slices).


-spec shuffle( [ term() ] ) -> [ term() ].
shuffle(Lst) -> [ X || {_, X} <- lists:sort([ {random:uniform(), N} || N <- Lst ]) ].


-spec thousand_separator( integer() ) -> string().
%% thousand_separator(1234567) => "1,234,567"
thousand_separator(Num) -> {Res, _} = lists:foldr(fun(A, {Acc, 3}) -> {[A|[$,|Acc]], 1};
                                                     (A, {Acc, N}) -> {[A|Acc], N+1} end,
                                                  {[],0},
                                                  integer_to_list(Num)),
                           Res.


-spec current_time_us() -> integer().
current_time_us() -> {MegaSecs, Secs, MicroSecs} = erlang:now(),
                     (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.


%%%
%%% Main logic
%%%

-spec attack( [ integer() ], integer()) -> ok.
attack(Numbers, Result) ->
    StartTime  = current_time_us(),
    Printer    = spawn(fun() -> printer(StartTime, 0) end),
    UniqSlices = uniq_slices(Numbers),
    Self       = self(),
    Pids       = [ spawn(fun() -> brutus(Printer, Result, NumbersLeft, N, integer_to_list(N), N),
                                  Self ! {done, self()}
                         end)
                   || {N, NumbersLeft} <- UniqSlices ],
    print_debug("~p threads started: ~p", [length(Pids), Pids]),
    [ receive
          {done, Pid} -> print_debug("thread ~p done", [Pid])
      end
      || _ <- UniqSlices ],
    Time = (current_time_us() - StartTime) / 1000000,
    print_debug("all done after ~.3fs.", [Time]),
    Printer ! done.


printer(StartTime, BestScore) ->
    receive
        %% print better solutions
        {solution, Str, Score, Sender}
          when Score > BestScore -> StrClean = case Str of
                                                   [$(|_] -> lists:sublist(Str, 2, length(Str)-2);
                                                   _ -> Str
                                               end,
                                    io:format("~s~n", [StrClean]),
                                    Time = (current_time_us() - StartTime) / 1000000,
                                    ScoreStr = thousand_separator(Score),
                                    ScoreFill = lists:duplicate(max(0,11-length(ScoreStr)),$ ),
                                    print_debug("[t=~7.3fs, p=~p, k=~s~s]  ~s", [Time, Sender, ScoreFill, ScoreStr, StrClean]),
                                    printer(StartTime, Score);
        %% discard worse solutions
        {solution, _, _, _}      -> printer(StartTime, BestScore);
        %% stop this process
        done                     -> ok;
        %% fallback
        Unknown                  -> print_debug("WARN: printer process received unknown message: ~p", [Unknown]),
                                    printer(StartTime, BestScore)
    end.


-ifdef(modulo).

print_macro_info() -> print_debug("MIT MODULO ;D", []).

brutus(Printer, Result, Numbers, A, AStr, K) ->
    case A == Result of
        true -> Printer ! {solution, AStr, K, self()};
        _    -> ok
    end,
    Brutus = fun(B, NewNumbers, NewA, NewAStr) ->
                     brutus(Printer, Result, shuffle(NewNumbers), NewA, "("++NewAStr++")", K + B + NewA)
             end,
    [ begin BStr = integer_to_list(B),
            AmodB = A rem B,
            BmodA = B rem A,
            AgtB  = A > B,
            BgtA  = B > A,
                                 Brutus(B, NumbersLeft, (A+B)*(A+B), BStr++"<+>"++AStr),
            AgtB         andalso Brutus(B, NumbersLeft, (A+B)*(A-B), AStr++"<*>"++BStr),
            BgtA         andalso Brutus(B, NumbersLeft, (B+A)*(B-A), BStr++"<*>"++AStr),
            (AmodB == 0) andalso Brutus(B, NumbersLeft,   A div B,    AStr++"/"++BStr),
            (BmodA == 0) andalso Brutus(B, NumbersLeft,   B div A,    BStr++"/"++AStr),
                                 Brutus(B, NumbersLeft,    A * B,     BStr++"*"++AStr),
                                 Brutus(B, NumbersLeft,    A + B,     BStr++"+"++AStr),
            (AmodB > 0)  andalso Brutus(B, NumbersLeft,    AmodB,     AStr++"%"++BStr),
            (BmodA > 0)  andalso Brutus(B, NumbersLeft,    BmodA,     BStr++"%"++AStr),
            AgtB         andalso Brutus(B, NumbersLeft, (A-B)*(A-B), AStr++"<->"++BStr),
            BgtA         andalso Brutus(B, NumbersLeft, (B-A)*(B-A), BStr++"<->"++AStr),
            AgtB         andalso Brutus(B, NumbersLeft,    A - B,     AStr++"-"++BStr),
            BgtA         andalso Brutus(B, NumbersLeft,    B - A,     BStr++"-"++AStr)
      end
      || {B, NumbersLeft} <- uniq_slices(Numbers) ].

-else.

print_macro_info() -> print_debug("ohne modulo ;-((", []).

brutus(Printer, Result, Numbers, A, AStr, K) ->
    case A == Result of
        true -> Printer ! {solution, AStr, K, self()};
        _    -> ok
    end,
    Brutus = fun(B, NewNumbers, NewA, NewAStr) ->
                     brutus(Printer, Result, shuffle(NewNumbers), NewA, "("++NewAStr++")", K + B + NewA)
             end,
    [ begin BStr = integer_to_list(B),
            AmodB = A rem B,
            BmodA = B rem A,
            AgtB  = A > B,
            BgtA  = B > A,
                                 Brutus(B, NumbersLeft, (A+B)*(A+B), BStr++"<+>"++AStr),
            AgtB         andalso Brutus(B, NumbersLeft, (A+B)*(A-B), AStr++"<*>"++BStr),
            BgtA         andalso Brutus(B, NumbersLeft, (B+A)*(B-A), BStr++"<*>"++AStr),
            (AmodB == 0) andalso Brutus(B, NumbersLeft,   A div B,    AStr++"/"++BStr),
            (BmodA == 0) andalso Brutus(B, NumbersLeft,   B div A,    BStr++"/"++AStr),
                                 Brutus(B, NumbersLeft,    A * B,     BStr++"*"++AStr),
                                 Brutus(B, NumbersLeft,    A + B,     BStr++"+"++AStr),
            AgtB         andalso Brutus(B, NumbersLeft, (A-B)*(A-B), AStr++"<->"++BStr),
            BgtA         andalso Brutus(B, NumbersLeft, (B-A)*(B-A), BStr++"<->"++AStr),
            AgtB         andalso Brutus(B, NumbersLeft,    A - B,     AStr++"-"++BStr),
            BgtA         andalso Brutus(B, NumbersLeft,    B - A,     BStr++"-"++AStr)
      end
      || {B, NumbersLeft} <- uniq_slices(Numbers) ].

-endif.
