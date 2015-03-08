%% ----------------------------------------------------------------------------
%% @doc Defines the central process of the framework.
%% This is a finite state machine handling the game state
%% (e.g. "round running") as well as handling the communication
%% between worker nodes, GUI, validator, state changer and barkeeper.
%%
%% == State machine states ==
%%
%% <dl>
%%
%%   <dt>ready</dt>
%%   <dd>This is the initial state. It implies that all prerequisites
%%       for starting a round are met. When a round is successfully
%%       started (see {@link dj:start_round/0}), the fsm transitions
%%       into the <tt>running</tt> state.</dd>
%%
%%   <dt>running</dt>
%%   <dd>The <tt>running</tt> state indicates that a round is
%%       currently running. Only in this state, the dj accepts new
%%       propositions from the workers. When all the workers are
%%       stopped, it transitions to <tt>waiting_for_validator</tt>.</dd>
%%
%%   <dt>waiting_for_validator</dt>
%%   <dd>When the round is over and the workers have stopped, buffers
%%       and queues can still contain valid propositions. The
%%       validator queue is notified of the end of the round, processes
%%       all propositions that reached it and then in turn notifies
%%       the dj.<br/>
%%       This is done by calling {@link dj:validator_queue_empty/0},
%%       which prompts the dj to transition into the state <tt>ready</tt>.</dd>
%%
%% </dl>
%% @end
%% ----------------------------------------------------------------------------

-module(dj).

-behavior(gen_fsm).

-include("dj_data.hrl").

%% public API
-export([
         start_link/0,
         choose_problem/1,
         start_round/0,
         kill_all_workers/0,
         submit_proposition/2,
         worker_stopped/1,
         submit_validated_proposition/5,
         gui_wants_all_data/0,
         load_game_state/1,
         save_game_state/1,
         apply_proposition/1,
         block_worker/1,
         unblock_worker/1,
         add_scores/0,
         quit_program/0,
         validator_queue_empty/0
        ]).

%% called by gen_fsm module
-export([
         init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4,

         %% these get called for incoming messages
         %% according to the current state
         ready/2,
         ready/3,
         running/2,
         running/3,
         waiting_for_validator/2,
         waiting_for_validator/3
        ]).

%% name under which to register
-define(REG_NAME, {global, dj}).

%% ===================================================================
%% public application programming interface
%% ===================================================================

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
start_link() ->
    gen_fsm:start_link(?REG_NAME, ?MODULE, [], []).

%% called by gui:

-spec choose_problem(non_neg_integer()) -> ok.
choose_problem(ProblemIdx) ->
    gen_fsm:send_event(?REG_NAME, {choose_problem, ProblemIdx}).

-spec start_round() -> ok.
start_round() ->
    gen_fsm:send_event(?REG_NAME, start_round).

%% @doc Send a kill message to all workers. This ends the currently
%% running round.
-spec kill_all_workers() -> ok.
kill_all_workers() ->
    gen_fsm:send_all_state_event(?REG_NAME, kill_all_workers).

-spec load_game_state(file:filename()) -> ok.
load_game_state(FilePath) ->
    gen_fsm:send_event(?REG_NAME, {load_game_state, FilePath}).

-spec save_game_state(file:filename()) -> ok.
save_game_state(FilePath) ->
    gen_fsm:send_event(?REG_NAME, {save_game_state, FilePath}).

-spec gui_wants_all_data() -> ok.
gui_wants_all_data() ->
    gen_fsm:send_all_state_event(?REG_NAME, gui_wants_all_data).

%% @doc Change the problem state according to the last proposition of the indicated worker.
%% This is used to play multiple rounds with a single problem spec.
-spec apply_proposition(worker_id()) -> ok.
apply_proposition(WorkerID) ->
    gen_fsm:send_event(?REG_NAME, {apply_proposition, WorkerID}).

-spec block_worker(worker_id()) -> ok.
block_worker(WorkerID) ->
    gen_fsm:send_all_state_event(?REG_NAME, {block_worker, WorkerID}).

-spec unblock_worker(worker_id()) -> ok.
unblock_worker(WorkerID) ->
    gen_fsm:send_all_state_event(?REG_NAME, {unblock_worker, WorkerID}).

%% @doc Accumulate current scores per worker. Each worker has the score for his last
%% proposition and a total accumulated score for the current problem.
-spec add_scores() -> ok.
add_scores() ->
    gen_fsm:send_event(?REG_NAME, add_scores).

-spec quit_program() -> ok.
quit_program() ->
    gen_fsm:send_all_state_event(?REG_NAME, quit_program).

%% worker callbacks:

-spec submit_proposition(worker_id(), string()) -> ok.
submit_proposition(WorkerID, WorkerOutput) ->
    gen_fsm:send_event(?REG_NAME, {submit_proposition, WorkerID, WorkerOutput}).

-spec worker_stopped(worker_id()) -> ok.
worker_stopped(WorkerID) ->
    gen_fsm:send_event(?REG_NAME, {worker_stopped, WorkerID}).

%% validator callback:

-spec submit_validated_proposition(worker_id(), string(), string(), integer(), string()) -> ok.
submit_validated_proposition(WorkerID, WorkerInput, WorkerOutput, Score, Caption) ->
    gen_fsm:send_event(?REG_NAME, {submit_validated_proposition, WorkerID, WorkerInput, WorkerOutput, Score, Caption}).

-spec validator_queue_empty() -> ok.
validator_queue_empty() ->
    gen_fsm:send_event(?REG_NAME, validator_queue_empty).

%% ===================================================================
%% gen_fsm callbacks
%% ===================================================================


init(_) ->
    {ok, Problems         } = application:get_env(problems),
    {ok, WorkerSpecs      } = application:get_env(workers),
    {ok, WorkerCallTimeout} = application:get_env(worker_call_timeout),
    {ok, ScoreMode        } = application:get_env(score_mode),

    WorkerDict = dict:from_list(
                   lists:map(
                     fun({WorkerID, _IP, RankingGroup}) ->
                             {WorkerID,
                              %% TODO exception handling for timeout
                              #worker{name = worker_app:get_worker_name(WorkerID,
                                                                        WorkerCallTimeout),
                                      ranking_group=RankingGroup}
                             }
                     end,
                     WorkerSpecs)),

    %% initialize with first problem spec
    [{_Description, ProblemSpec, _AnswerTime, StartState}|_] = Problems,

    case barkeeper:ask(ProblemSpec, StartState) of
        {ok, WorkerInput} ->
            ok = lager:info("New Worker input: ~p", [WorkerInput]),

            {ok, ready, #state{config=#config{problems=Problems,
                                              worker_call_timeout=WorkerCallTimeout,
                                              score_mode=ScoreMode},
                               worker_input=WorkerInput,
                               workers=WorkerDict,
                               problem_state=StartState}};
        {error, Error} ->
            ok = lager:critical("DJ got error from barkeeper while initializing with first problem: ~p", [Error]),
            {stop, "Critical problem while asking barkeeper for workerinput for first problem"}
    end.


%% -- state callbacks --


ready({save_game_state, FilePath}, Data) ->

    case savegames:save_state(FilePath, Data) of
        ok ->
            ok = lager:debug("Saved game state to ~p", [FilePath]),
            gui:ack_save_game_state(ok);
        {error, Error} ->
            ok = lager:error("While saving game state: ~p", [Error]),
            gui:ack_save_game_state(Error)
    end,
    {next_state, ready, Data};


ready({load_game_state, FilePath}, Data=#state{config=#config{problems=Problems,
                                                              score_mode=ScoreMode},
                                               workers=WorkerDict}) ->

    NewData =
        case file:consult(FilePath) of
            {ok, SaveGame} ->
                case savegames:validate(SaveGame, Problems, ScoreMode, _WorkerIDs=dict:fetch_keys(WorkerDict)) of
                    true ->
                        ok = lager:debug("Loaded game state from ~p", [FilePath]),
                        gui:ack_load_game_state(ok),
                        TransformedData = savegames:apply(SaveGame, Data),
                        send_all_data_to_gui(ready, TransformedData),
                        TransformedData;
                    false ->
                        ok = lager:error("Format error while loading game state from file ~p", [FilePath]),
                        gui:ack_load_game_state(eformat),
                        Data
                end;
            {error, Tuple={_Line, _Mod, _Term}} ->
                ok = lager:error("Syntax error while loading game state: ~p",
                            [file:format_error(Tuple)]),
                gui:ack_load_game_state(eformat),
                Data;
            {error, Error} ->
                ok = lager:error("while loading game state: ~p", [Error]),
                gui:ack_load_game_state(Error),
                Data
    end,
    {next_state, ready, NewData};


%% Change the problem state according to the last proposition of the indicated worker
ready({apply_proposition, WorkerID}, Data=#state{config=#config{problems=Problems},
                                                 workers=WorkerDict,
                                                 problem_state=OldProblemState,
                                                 problem_idx=ProblemIdx}) ->

    #worker{last_proposition=LastProposition} = dict:fetch(WorkerID, WorkerDict),
    case LastProposition of
        none ->
            ok = lager:info("Refusing to apply proposition: Worker ~p has not submitted anything yet.", [WorkerID]),
            {next_state, ready, Data};
        _ ->
            case changer:change_state(OldProblemState, LastProposition) of
                {ok, NewProblemState} ->
                    {_Description, ProblemSpec, _AnswerTime, _StartState} =
                        lists:nth(ProblemIdx+1, Problems),

                    %% ask for new worker input (challenge) based on the new problem state
                    case barkeeper:ask(ProblemSpec, NewProblemState) of
                        {ok, WorkerInput} ->
                            ok = lager:info("New Worker input: ~p", [WorkerInput]),

                            gui:problem_state_changed(NewProblemState),
                            gui:worker_input_changed(WorkerInput),
                            {next_state, ready, Data#state{worker_input=WorkerInput,
                                                           problem_state=NewProblemState}};
                        {error, Error} ->
                            ok = lager:error("DJ got error from barkeeper while applying proposition: ~p", [Error]),
                            %% do nothing, let user try again
                            {next_state, ready, Data}
                    end;
                Error ->
                    ok = lager:error("Error applying proposition: ~p. Worker ID: ~p, Error: ~p", [LastProposition, WorkerID, Error]),
                    %% do nothing, let user try again
                    {next_state, ready, Data}
            end
    end;


ready(add_scores, Data=#state{workers=WorkerDict}) ->

    NewWorkerDict = dict:map(
                      fun(WorkerID, W=#worker{last_prop_processed_score=ThisProcScore,
                                              problem_score=ProbScore,
                                              blocked=Blocked}) ->
                              case Blocked of
                                  no ->
                                      NewWorker = W#worker{problem_score=ProbScore + ThisProcScore},
                                      send_worker_to_gui(WorkerID, NewWorker),
                                      NewWorker;
                                  _ -> W
                              end
                      end,
                      WorkerDict),
    {next_state, ready, Data#state{workers=NewWorkerDict}};


ready({worker_stopped, WorkerID}, Data) ->

    ok = lager:debug("DJ received worker_stopped for worker ~p while in state ready", [WorkerID]),
    %% ignore this event
    {next_state, ready, Data};


ready({choose_problem, ProblemIdx}, Data=#state{config=#config{problems=Problems}, workers=WorkerDict}) ->

    try lists:nth(ProblemIdx+1, Problems) of
        {_Description, ProblemSpec, _AnswerTime, StartState} ->
            case barkeeper:ask(ProblemSpec, StartState) of
                {ok, WorkerInput} ->
                    ok = lager:info("problem ~p chosen", [ProblemIdx]),
                    ok = lager:info("New Worker input: ~p", [WorkerInput]),

                    gui:problem_chosen(ProblemIdx),
                    gui:problem_state_changed(StartState),
                    gui:worker_input_changed(WorkerInput),

                    {next_state, ready, Data#state{problem_idx=ProblemIdx,
                                                   worker_input=WorkerInput,
                                                   round=0,
                                                   problem_state=StartState,
                                                   workers=WorkerDict}};
                {error, Error} ->
                    ok = lager:error("DJ got error from barkeeper while choosing problem: ~p", [Error]),
                    %% do nothing, let user try again
                    {next_state, ready, Data}
            end
    catch
        _ ->
            ok = lager:error("invalid request: choose_problem with index ~p", [ProblemIdx]),
            {next_state, ready, Data}
    end;


ready(start_round, Data=#state{config=#config{problems=Problems, worker_call_timeout=WorkerCallTimeout},
                               workers=WorkerDict,
                               problem_idx=ProblemIdx,
                               worker_input=WorkerInput,
                               round=Round}) ->

    case dict:size(get_unblocked_workers(WorkerDict)) of
        0 ->
            ok = lager:info("DJ received start_round but no unblocked workers are registered"),
            {next_state, ready, Data};
        _Some ->
            NewRound = Round+1,
            ok = lager:info("==> Starting problem ~p in round ~p with workers:~n    ~p",
                            [ProblemIdx, NewRound, [ WId || {WId, _WData} <- dict:to_list(WorkerDict) ]]),
            {_Description, _ProblemSpec, AnswerTime, _StartState} = lists:nth(ProblemIdx+1, Problems),

            %% the challenge is given to the workers and triggers the timers in the worker nodes so that
            %% each worker gets killed at the right time.
            try give_challenges(WorkerDict, WorkerInput, AnswerTime, WorkerCallTimeout) of
                NewWorkerDict ->
                    gui:round_started(NewRound),
                    validator:round_started(),

                    {next_state, running, Data#state{workers=NewWorkerDict,
                                                     round=NewRound,
                                                     worker_input=WorkerInput}}
            catch
                _ ->
                    %% round could not be started
                    %% reset all workers, let user try starting a round again
                    kill_all_workers(ready, WorkerDict),
                    ok = lager:error("Not starting round because of error in giving out challenges"),
                    {next_state, ready, Data}
            end
        end;


ready(Event, Data) ->

    unexpected(Event, ready),
    {next_state, ready, Data}.



ready(Event, _From, Data) ->

    unexpected(Event, ready),
    {reply, unexpected, ready, Data}.


%% -----


%% a worker submits a proposition
running({submit_proposition, WorkerID, WorkerOutput}, Data=#state{workers=WorkerDict, worker_input=WorkerInput}) ->

    case dict:find(WorkerID, WorkerDict) of
        {ok, #worker{blocked=no}} ->
            validator:validate(WorkerID, WorkerInput, WorkerOutput);
        {ok, _} ->
            %% ignore propositions from blocked workers
            ok;
        error ->
            ok = lager:error("received proposition from unknown worker ~p", [WorkerID])
    end,
    {next_state, running, Data};


%% the validator submits the result of validating a proposition
running({submit_validated_proposition, WorkerID, WorkerInput, WorkerOutput, Score, Caption},
        Data=#state{worker_input=WorkerInput, workers=WorkerDict}) ->

    NewWorkerDict = process_validated_proposition(WorkerID, WorkerOutput, Score, Caption, WorkerDict),
    {next_state, running, Data#state{workers=NewWorkerDict}};


running({worker_stopped, WorkerID}, Data=#state{workers=WorkerDict}) ->

    NewWorkerDict = update_worker(WorkerID,
                                  fun(Worker) ->
                                          Worker#worker{working=false}
                                  end,
                                  WorkerDict),
    ok = lager:info("worker ~p stopped", [WorkerID]),
    RunningWorkers = dict:filter(fun(_, #worker{working=Working, blocked=Blocked}) ->
                                         (Blocked == no) and Working
                                 end,
                                 NewWorkerDict),
    NewData = Data#state{workers=NewWorkerDict},
    NextState = case dict:size(RunningWorkers) of
                    0 ->
                        %% when all workers are stopped, we wait for the validator
                        %% to finish queued propositions before ending the round.
                        ok = lager:info("all workers stopped. waiting for validator"),
                        validator:all_workers_stopped(),
                        waiting_for_validator;
                    _ ->
                        running
                end,
    {next_state, NextState, NewData};


running(Event, Data) ->

    unexpected(Event, running),
    {next_state, running, Data}.



running(Event, _From, Data) ->

    unexpected(Event, running),
    {reply, unexpected, running, Data}.


%% -----


%% validator tells us he has finished processing propositions so we end
%% the round
waiting_for_validator(validator_queue_empty, Data=#state{round=Round,
                                                         workers=WorkerDict,
                                                         problem_idx=ProblemIdx,
                                                         problem_state=ProblemState,
                                                         config=#config{
                                                                   problems=Problems,
                                                                   score_mode=ScoreMode}}) ->

    ok = lager:info("Validator queue empty. ending round ~p", [Round]),
    gui:round_ended(Round),

    NormData = Data#state{workers=calc_and_send_normalized_scores(ScoreMode, WorkerDict)},

    case savegames:autosave(NormData) of
        disabled ->
            ok;
        {ok, Path} ->
            ok = lager:info("Autosaved game state to ~s", [Path]);
        {error, SaveError} ->
            ok = lager:error("While autosaving game state: ~p", [SaveError]),
            gui:ack_save_game_state(SaveError)
    end,

    %% calculate next worker input to be able to play another round
    %% with this problem
    {_Description, ProblemSpec, _AnswerTime, _StartState} =
        lists:nth(ProblemIdx+1, Problems),

    case barkeeper:ask(ProblemSpec, ProblemState) of
        {ok, WorkerInput} ->
            ok = lager:info("New Worker input: ~p", [WorkerInput]),

            gui:worker_input_changed(WorkerInput),
            {next_state, ready, NormData#state{worker_input=WorkerInput}};

        {error, Error} ->
            ok = lager:critical("DJ got error from barkeeper while ending round: ~p", [Error]),
            %% do nothing, let user try again
            {next_state, ready, Data}
    end;


waiting_for_validator({submit_validated_proposition, WorkerID, WorkerInput, WorkerOutput, Score, Caption},
        Data=#state{worker_input=WorkerInput, workers=WorkerDict}) ->

    NewWorkerDict = process_validated_proposition(WorkerID, WorkerOutput, Score, Caption, WorkerDict),
    {next_state, waiting_for_validator, Data#state{workers=NewWorkerDict}};


waiting_for_validator(Event, Data) ->

    unexpected(Event, waiting_for_validator),
    {next_state, waiting_for_validator, Data}.



waiting_for_validator(Event, _From, Data) ->

    unexpected(Event, waiting_for_validator),
    {reply, unexpected, waiting_for_validator, Data}.


%% -- stateless callbacks --


handle_event(kill_all_workers, State, Data=#state{workers=WorkerDict}) ->

    %% this generates worker_stopped messages from the workers
    %% And thus ends the round indirectly.
    kill_all_workers(State, WorkerDict),
    %% do not change state
    {next_state, State, Data};


handle_event({block_worker, WorkerID}, State, Data=#state{workers=WorkerDict}) ->

    ok = lager:debug("blocking worker ~p", [WorkerID]),
    worker_app:kill_worker(WorkerID),
    MaxBlockIdx = get_max_block_idx(WorkerDict),
    NewWorkerDict = update_worker(WorkerID,
                                  fun(W) ->
                                          W#worker{blocked={idx, MaxBlockIdx+1}}
                                  end,
                                  WorkerDict),
    {next_state, State, Data#state{workers=NewWorkerDict}};


handle_event({unblock_worker, WorkerID}, State, Data=#state{workers=WorkerDict}) ->

    ok = lager:debug("unblocking worker ~p", [WorkerID]),
    NewWorkerDict = update_worker(WorkerID,
                                  fun(W) ->
                                          W#worker{blocked=no}
                                  end,
                                  WorkerDict),
    {next_state, State, Data#state{workers=NewWorkerDict}};


handle_event(gui_wants_all_data, State, Data) ->

    send_all_data_to_gui(State, Data),
    {next_state, State, Data};


handle_event(quit_program, State, Data) ->

    ok = lager:info("Quitting program"),

    case init:get_argument(cover_enabled) of
        {ok, [["true"]]} -> cover_utils:analyze();
        _                -> ok
    end,

    init:stop(),
    {next_state, State, Data};


handle_event(Event, State, Data) ->

    unexpected(Event, State),
    {next_state, State, Data}.


%% -----


handle_sync_event(Event, _From, State, Data) ->

    unexpected(Event, State),
    {reply, unexpected, State, Data}.


%% -----


handle_info(Info, State, Data) ->

    unexpected(Info, State),
    {next_state, State, Data}.


%% -----


%% @doc Only really necessary for hot code loading, but has to be declared
%% for the gen_fsm behavior.
%% We don't do hot code loading so this is just a dummy.
code_change(_OldVsn, DataName, Data, _Extra) ->

    %% No change planned.
    {ok, DataName, Data}.


%% -----


terminate(_Msg, _StateName, _Data) ->

    ok.

%% ===================================================================
%% private functions
%% ===================================================================

%% @doc calculate normalized scores for the ending round and send
%% corresponding updates to gui
-spec calc_and_send_normalized_scores(score_mode(), worker_dict()) -> worker_dict().
calc_and_send_normalized_scores(ScoreMode, WorkerDict) ->
    ProcScoreFun =
        case ScoreMode of
            raw ->
                fun(Score) -> Score end;
            normalized ->
                MaxScore = lists:max(lists:map(fun({_WId, #worker{last_prop_score=Score}}) ->
                                                       Score
                                               end,
                                               dict:to_list(WorkerDict))),
                fun(Score) -> (Score * 100) div MaxScore end;
            ranked ->
                UnBlockedWorkers = lists:filter(fun({_,#worker{blocked=no}}) -> true;
                                                   (_) -> false end,
                                                dict:to_list(WorkerDict)),
                case UnBlockedWorkers of
                    [] ->
                        fun(Score) -> Score end;
                    _ ->
                        [LowestScore|RestSortedScores] =
                            lists:sort(
                              lists:map(fun({_WId, #worker{last_prop_score=Score}}) ->
                                                Score
                                        end,
                                        UnBlockedWorkers)),
                        Ranking = lists:foldl(fun(Score, Acc=[{LastScore, LastRank}|Rest]) ->
                                                      if
                                                          Score == LastScore ->
                                                              [{LastScore, LastRank+1}|Rest];
                                                          true ->
                                                              [{Score, LastRank+1}|Acc]
                                                      end
                                              end,
                                              [{LowestScore, 1}],
                                              RestSortedScores),
                        fun(Score) ->
                                {Score, Rank} = proplists:lookup(Score, Ranking),
                                Rank
                        end
                end
        end,
    NewWorkerDict = dict:map(fun(_, Worker=#worker{last_prop_score=Score,
                                                   blocked=Blocked}) ->
                                     case Blocked of
                                         no ->
                                             Worker#worker{
                                               last_prop_processed_score =
                                                   apply(ProcScoreFun, [Score])
                                              };
                                         {idx, _} -> Worker
                                     end
                             end,
                             WorkerDict),
    _ = dict:map(fun send_worker_to_gui/2, NewWorkerDict),
    NewWorkerDict.


%% @doc Unexpected allows to log unexpected messages
-spec unexpected(term(), atom()) -> any().
unexpected(Msg, State) ->

    ok = lager:error("~p received unknown event ~p while in state ~p",
                [self(), Msg, State]).


%% @doc Update a single worker in the worker dict.
-spec update_worker(worker_id(), fun((#worker{}) -> #worker{}), worker_dict()) -> worker_dict().
update_worker(WorkerID, Fun, WorkerDict) ->

    dict:update(WorkerID,
                fun(Worker) ->
                        NewWorker = Fun(Worker),
                        send_worker_to_gui(WorkerID, NewWorker),
                        NewWorker
                end,
                WorkerDict).


%% @doc Send the state of a single worker to the gui.
-spec send_worker_to_gui(worker_id(), #worker{}) -> ok.
send_worker_to_gui(WorkerID, #worker{
                                last_proposition=Proposition,
                                last_prop_caption=Caption,
                                last_prop_score=Score,
                                last_prop_processed_score=ProcScore,
                                problem_score=ProbScore,
                                blocked=Blocked,
                                working=Working
                               }) ->

    gui:worker_updated(WorkerID,
                       Proposition,
                       Caption,
                       Score,
                       ProcScore,
                       ProbScore,
                       Blocked,
                       Working).


%% @doc Give all workers the next challenge. Thereby starting the round
%% timer in the worker_app.
%% @throws {port_unreachable, WorkerID} | {timeout, WorkerID}
-spec give_challenges(worker_dict(), [string()], pos_integer(), non_neg_integer())
                     -> worker_dict().
give_challenges(WorkerDict, WorkerInput, AnswerTime, Timeout) ->
    dict:map(
      fun(WorkerID, Worker=#worker{blocked=Blocked}) ->
              case Blocked of
                  no ->
                      try worker_app:give_worker_challenge(WorkerID, WorkerInput, AnswerTime, Timeout) of
                          ok ->
                              ChangedWorker = Worker#worker{last_proposition=none,
                                                            last_prop_score=0,
                                                            last_prop_processed_score=0,
                                                            last_prop_caption="",
                                                            working=true},
                              send_worker_to_gui(WorkerID, ChangedWorker),
                              ChangedWorker;
                          {error, Error} ->
                              ok = lager:error("worker ~p: port unreachable, "
                                               "Error: ~p", [WorkerID, Error]),
                              throw({port_unreachable, WorkerID})
                      catch
                          exit:{timeout, Info} ->
                              ok = lager:error("Timeout in gen_server:call on worker ~p. "
                                               "Message: ~p", [WorkerID, Info]),
                              throw({timeout, WorkerID});
                          Type:Exception -> ok = lager:error("Exception in gen_server:call on worker ~p. "
                                                  "Message: ~p ~p", [WorkerID, Type, Exception]),
                                            throw({exception, WorkerID, Type, Exception})
                      end;
                  _ ->
                      Worker
              end
      end,
      WorkerDict
     ).


%% @doc Full gui data update. Send all data the gui accepts in one message.
-spec send_all_data_to_gui(atom(), Data :: #state{}) -> any().
send_all_data_to_gui(State, #state{config=#config{problems=Problems},
                                   workers=WorkerDict,
                                   problem_idx=ProblemIndex,
                                   round=RoundNumber,
                                   problem_state=ProblemState,
                                   worker_input=WorkerInput
                                  }) ->

    GuiWorkerList = lists:map(
                      fun({ID, #worker{name=Name,
                                       last_proposition=Proposition,
                                       last_prop_caption=Caption,
                                       last_prop_score=Score,
                                       last_prop_processed_score=ProcScore,
                                       problem_score=ProblemScore,
                                       blocked=Blocked,
                                       working=Working,
                                       ranking_group=RankingGroup
                                      }}) ->
                              {ID, Name, RankingGroup, Proposition, Caption, Score, ProcScore, ProblemScore, Blocked, Working}
                      end,
                      dict:to_list(WorkerDict)
                     ),
    GuiProblemList = lists:map(
                       fun({Desc, Spec, AnswerTime, StartState}) ->
                               {Desc, Spec, AnswerTime, StartState}
                       end,
                       Problems
                      ),
    Running = State == running,
    gui:all_data(Running, GuiWorkerList, GuiProblemList, ProblemIndex, RoundNumber, ProblemState, WorkerInput).


%% @doc Send a kill message to all workers. They should all answer worker_stopped,
%% thereby ending the round, should one be running.
-spec kill_all_workers(atom(), worker_dict()) -> ok.
kill_all_workers(State, WorkerDict) ->

    ok = lager:info("DJ killing all workers in state ~p", [State]),

    lists:foreach(
      fun({WorkerID, _}) ->
              worker_app:kill_worker(WorkerID)
      end,
      dict:to_list(WorkerDict)
     ),
    ok.


%% @doc Change worker according to the validation result of his proposition.
%% The proposition is saved as his last proposition and in the case that the
%% proposition is invalid the worker is blocked.
-spec process_validated_proposition(worker_id(), string(), integer(), string(), worker_dict()) -> worker_dict().
process_validated_proposition(WorkerID, WorkerOutput, Score, Caption, WorkerDict) ->

    MaxBlockIdx = get_max_block_idx(WorkerDict),
    case dict:find(WorkerID, WorkerDict) of
        {ok, #worker{blocked=no}} ->
            Blocked = case Score of
                          Val when Val < 0 ->
                              ok = lager:info("Worker ~p blocked because of invalid proposition ~p",
                                         [WorkerID, WorkerOutput]),
                              worker_app:kill_worker(WorkerID),
                              {idx, MaxBlockIdx + 1};
                          _ -> no
                      end,
            RealScore = case Blocked of
                            {idx, _} ->
                                0;
                            no ->
                                Score
                        end,
            update_worker(WorkerID,
                          fun(Worker) ->
                                  Worker#worker{last_proposition=WorkerOutput,
                                                last_prop_score=RealScore,
                                                last_prop_caption=Caption,
                                                blocked=Blocked}
                          end,
                          WorkerDict);
        {ok, _} ->
            %% ignore propositions from blocked workers
            WorkerDict;
        error ->
            ok = lager:error("received validated proposition from unknown worker ~p", [WorkerID]),
            WorkerDict
    end.


%% @doc Return only the workers that are not blocked.
-spec get_unblocked_workers(worker_dict()) -> worker_dict().
get_unblocked_workers(WorkerDict) ->
    dict:filter(fun(_WorkerID,#worker{blocked=Blocked}) -> Blocked == no end, WorkerDict).


%% @doc Return the highest blocking index that is currently in use.
%% When workers are blocked they get a blocking index to later determine the
%% order in which workers where blocked. This function returns the highest block
%% index so the next blocked worker could get the next index.
-spec get_max_block_idx(worker_dict()) -> non_neg_integer().
get_max_block_idx(WorkerDict) ->
    dict:fold(fun(_Key, #worker{blocked=Blocked}, Max) ->
                      case Blocked of
                          no ->
                              Max;
                          {idx, Idx} ->
                              max(Idx, Max)
                      end
              end,
              0,
              WorkerDict).

%% ===================================================================
