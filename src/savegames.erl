%% -------------------------------------------------------------------
%% @doc Utility functions for saving and loading the game state to/from disk.<br/>
%% Savegame files are created with {@link autosave/1} or {@link save_state/2}.
%% To load a savegame, the file should be read with {@link file:consult/1},
%% contents validated with {@link validate/3} and applied with {@link apply/2}.
%% @end
%% -------------------------------------------------------------------

-module(savegames).

-include("dj_data.hrl").

-export([
         autosave/1,
         save_state/2,
         validate/4,
         apply/2
        ]).

%% @doc Creates a new savegame file with an auto-generated name.<br/>
%% The files are created in the path specified by the env value 'autosave_dir'.
-spec autosave(#state{}) -> {ok, file:filename()} | {error, file:posix()} | disabled.
autosave(Data=#state{problem_idx=ProblemIndex, round=Round}) ->
    case make_autosave_path(ProblemIndex, Round) of
        {ok, FilePath} = Ok ->
            case save_state(FilePath, Data) of
                ok -> Ok;
                Error -> Error
            end;
        disabled ->
            disabled;
        Error={error, _} ->
            Error
    end.

%% @doc Creates a new savegame file at the specified location.
-spec save_state(file:filename(), #state{}) -> ok | {error, file:posix()}.
save_state(FilePath, #state{workers=WorkerDict,
                            config=#config{problems=Problems,
                                           score_mode=ScoreMode},
                            problem_idx=ProblemIndex,
                            round=RoundNumber,
                            problem_state=ProblemState,
                            worker_input=WorkerInput
                           }) ->

    WorkerList = dict:to_list(WorkerDict),
    SaveTuple = {WorkerList, ScoreMode, Problems, ProblemIndex, RoundNumber, ProblemState, WorkerInput},
    file:write_file(FilePath, serialize(SaveTuple)).

%% @doc Validates the contents of a savegame file against the current configuration.
-spec validate(SaveGame :: term(), list(problem()), score_mode(), list(worker_id())) ->
                      boolean().
validate([GameData = {WorkerList,
                      ScoreMode,
                      Problems,
                      ProblemIndex,
                      RoundNumber,
                      ProblemState,
                      WorkerInput},
          CRC],
         OldProblems,
         OldScoreMode,
         WorkerIDs) ->
    %% if the problem list has changed, ProblemIndex, RoundNumber, ProblemState
    %% and WorkerInput are irrelevant
    SameProblems = Problems == OldProblems,

    %% generate a list of test results
    ResultList =
        [
         case erlang:crc32(term_to_binary(GameData)) of
             CRC ->
                 ok;
             _ ->
                 {error, "CRC check failed"}
         end,

         case {config:valid_score_mode(ScoreMode), ScoreMode} of
             {true, OldScoreMode} ->
                 ok;
             {true, _} ->
                 {error, "score mode from savegame and current mode differ"};
             {false, _} ->
                 {error, "invalid score mode"}
         end,

         case is_list(WorkerList) of
             true ->
                 case [ W || W <- WorkerList, not valid_saved_worker(W) ] of
                     [] ->
                         case valid_unique_worker_ids(WorkerList) of
                             true ->
                                 %% if the loaded workers do not correspond to the saved workers, everything is irrelevant too
                                 SortedWorkerIDs = lists:sort(WorkerIDs),
                                 case lists:sort(dict:fetch_keys(dict:from_list(WorkerList))) of
                                     SortedWorkerIDs ->
                                         ok;
                                     Other ->
                                         {error, io_lib:format("Worker configuration doesn't fit the one in the savegame: ~p vs. ~p", [Other, WorkerIDs])}
                                 end;
                             false ->
                                 {error, "worker ids not unique"}
                         end;
                     ProblemList ->
                         {error, io_lib:format("illegal saved workers: ~p", [ProblemList])}
                 end;
             false ->
                 {error, "saved workerList is not a list"}
         end,

         case is_list(Problems) of
             true ->
                 case [ P || P <- Problems, not valid_saved_problem(P) ] of
                     [] ->
                         ok;
                     ProblemList ->
                         {error, io_lib:format("illegal saved problems: ~p", [ProblemList])}
                 end;
             false ->
                 {error, "saved problem list is not a list"}
         end
        ]

        ++

        case SameProblems of
            true ->
                [
                 case is_integer(ProblemIndex) of
                     true ->
                         case (ProblemIndex >= 0) and (ProblemIndex < length(Problems)) of
                             true ->
                                 ok;
                             false ->
                                 {error, "Problem index out of range"}
                         end;
                     false ->
                         {error, "saved problem index is not an integer"}
                 end,

                 case is_integer(RoundNumber) of
                     true ->
                         case RoundNumber >= 0 of
                             true ->
                                 ok;
                             false ->
                                 {error, "round number out of range"}
                         end;
                     false ->
                         {error, "round number is not an integer"}
                 end,

                 case is_list(ProblemState) of
                     true ->
                         ok;
                     false ->
                         %% if it is'nt a list, it can't be a string ;-)
                         {error, "Problem state is not a string"}
                 end,

                 case is_list(WorkerInput) of
                     true ->
                         ok;
                     false ->
                         %% if it is'nt a list, it can't be a string ;-)
                         {error, "worker input is not a string"}
                 end
                ];
            false ->
                [ok]
        end,

    ErrorList = [ String || {error, String} <- ResultList ],
    case ErrorList of
        [] ->
            true;
        _ ->
            %% generate nice error message from error list
            ok = lager:error(utils:intersperse("\n - ", [ "Errors in save game:" | ErrorList ])),
            false
    end;
validate(_, _, _, _) ->
    false.


%% @doc Applies a savegame to the current game state.
-spec apply(term(), #state{}) -> #state{}.
apply(_Savegame = [_GameData = {WorkerList,
                                ScoreMode,
                                Problems,
                                ProblemIndex,
                                RoundNumber,
                                ProblemState,
                                WorkerInput},
                   _CRC],
      CurrentState=#state{config=#config{problems=OldProblems,
                                         score_mode=OldScoreMode}}
     ) ->

    SameSettings = (Problems == OldProblems) and (ScoreMode == OldScoreMode),
    case SameSettings of
        true ->
            CurrentState#state{workers=dict:from_list(WorkerList),
                               problem_idx=ProblemIndex,
                               round=RoundNumber,
                               problem_state=ProblemState,
                               worker_input=WorkerInput};
        false ->
            CurrentState#state{workers=dict:from_list(WorkerList)}
    end.


%% ===================================================================
%% private functions
%% ===================================================================


%% @doc Serializes an erlang term to a string, including a checksum at the end.<br/>
%% Used here for serializing the game data term.
serialize(GameData) ->
    io_lib:fwrite("~p.\n~p.\n", [GameData, erlang:crc32(term_to_binary(GameData))]).


%% @doc Validates a saved problem.
-spec valid_saved_problem(_) -> boolean().
valid_saved_problem(_Problem = {Description,
                                Spec,
                                AnswerTime,
                                StartState})
  when is_list(Description),
       is_list(Spec),
       is_integer(AnswerTime),
       AnswerTime > 0,
       is_list(StartState) ->
    true;
valid_saved_problem(_) ->
    false.


%% @doc Validates uniqueness in a list of worker IDs.
-spec valid_unique_worker_ids([any()]) -> boolean().
valid_unique_worker_ids(WorkerList) ->
    UniqueList = lists:usort(fun({ID1, _}, {ID2, _}) ->
                                     ID1 =< ID2
                             end,
                             WorkerList),
    length(UniqueList) == length(WorkerList).


%% @doc Validates a saved worker.
-spec valid_saved_worker(_) -> boolean().
valid_saved_worker(_Worker = {WorkerID, {worker,
                                         Name,
                                         LastProp,
                                         LastPropScore,
                                         ProcessedScore,
                                         Caption,
                                         ProblemScore,
                                         Working,
                                         RankingGroup,
                                         Blocked}})
  when is_atom(WorkerID),
       is_list(Name),
       is_list(LastProp) or (LastProp == none),
       is_integer(LastPropScore),
       is_integer(ProcessedScore),
       is_list(Caption),
       is_integer(ProblemScore),
       is_boolean(Working),
       is_list(RankingGroup) ->
    case Blocked of
        no -> true;
        {idx, Idx} when is_integer(Idx) and (Idx >= 0) -> true;
        _ -> false
    end;
valid_saved_worker(_) ->
    false.

%% @doc Generates a filename (including path) for a new autosave.<br/>
%% Reads path from environment var autosave_dir.
-spec make_autosave_path(non_neg_integer(), non_neg_integer())
                        -> {ok, file:filename()} | {error, file:posix()} | disabled.
make_autosave_path(ProblemIdx, Round) ->
    case application:get_env(autosave_dir) of
        {ok, disabled} ->
            disabled;
        {ok, AutoSaveDir} ->
            Dir = filename:absname(AutoSaveDir),
            DirStatus = case file:make_dir(Dir) of
                            ok ->
                                ok;
                            {error, eexist} ->
                                ok;
                            Error ->
                                Error
                        end,
            case DirStatus of
                ok ->
                    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime(),
                    {ok, Problems} = application:get_env(problems),
                    {Description, _Spec, _Time, _State} = lists:nth(ProblemIdx+1, Problems),
                    ProblemDesc = re:replace(Description, "[^a-zA-Z0-9.,-=()]", "", [global]),

                    DateString = io_lib:format("~w~2..0w~2..0w", [Year, Month, Day]),
                    TimeString = io_lib:format("~2..0w~2..0w~2..0w", [Hour, Minute, Second]),
                    ProbString = io_lib:format("p~w_r~w_~s", [ProblemIdx, Round, ProblemDesc]),

                    FileName = DateString ++ "_" ++ TimeString ++ "_" ++ ProbString ++ ".sav",
                    {ok, filename:join([Dir, FileName])};
                _ ->
                    DirStatus
            end
    end.
