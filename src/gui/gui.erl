%% -------------------------------------------------------------------
%% @doc Handles interaction with the GUI(s).<br/>
%% This module interfaces possibly multiple external GUIs to the rest
%% of the application. Each GUI has its own {@link gui_port} process.
%% The functions in this module handle the delivery of updates to all
%% the GUIs and deliver commands from all the GUIs to the {@link dj}.
%% <br/>
%% This module starts a single {@link gui_port_sup} which then starts
%% and supervises a list of {@link gui_port}s.
%% @end
%% -------------------------------------------------------------------

-module(gui).

-include("../global_types.hrl").

%% application programming interface
-export([
         start_link/1,
         worker_updated/8,
         round_started/1,
         round_ended/1,
         worker_input_changed/1,
         problem_chosen/1,
         problem_state_changed/1,
         ack_save_game_state/1,
         ack_load_game_state/1,
         all_data/7
        ]).

%% called by gui_port module
-export([
         handle_json/1,
         get_all_data/0
        ]).


%% ===================================================================
%% application programming interface
%% ===================================================================

%% @doc Start the gui port supervisor and all the GUIs.
-spec start_link([file:filename()]) -> {ok, pid()} | ignore | {error, _}.
start_link(ExtProgList) ->
    gui_port_sup:start_link(ExtProgList).

%% @doc Notify GUIs: A worker has changed.
%% Send updated worker to all GUIs.
-spec worker_updated(worker_id(), string() | none, string(), integer(), integer(), integer(), no | {idx, non_neg_integer()}, boolean()) -> ok.
worker_updated(WorkerID, Proposition, PropCaption, Score, ProcessedScore, ProblemScore, Blocked, Working) ->

    %% convert values to JSON-convertible types
    JSONProp = case Proposition of
                   none -> null;
                   PropString ->  list_to_binary(PropString)
               end,
    JSONBlock = case Blocked of
                    no -> utils:atom_to_binary(no);
                    {idx, Idx} -> [{<<"idx">>, Idx}]
                end,

    send_event_to_gui("worker updated",
                      [{"worker data",
                        [
                         utils:atom_to_binary(WorkerID),
                         JSONProp,
                         list_to_binary(PropCaption),
                         Score,
                         ProcessedScore,
                         ProblemScore,
                         JSONBlock,
                         Working
                        ]
                       }]
                     ).

%% @doc Notify GUIs: A round has been started.
-spec round_started(non_neg_integer()) -> ok.
round_started(RoundNumber) ->
    send_event_to_gui("round started",
                      [{"round number", RoundNumber}]
                     ).

%% @doc Notify GUIs: The round has ended.
-spec round_ended(non_neg_integer()) -> ok.
round_ended(RoundNumber) ->
    send_event_to_gui("round ended",
                      [{"round number", RoundNumber}]
                     ).

%% @doc Notify GUIs: The worker input for the next round has been generated.
%% Send the new worker input to all GUIs.
-spec worker_input_changed([string()]) -> ok.
worker_input_changed(InputList) ->
    send_event_to_gui("worker input changed",
                      [{"worker input", lists:map(fun list_to_binary/1, InputList)}]
                     ).

%% @doc Notify GUIs: A new problem is selected.
%% Send the new problem index to all GUIs.
-spec problem_chosen(non_neg_integer()) -> ok.
problem_chosen(ProblemIdx) ->
    send_event_to_gui("problem chosen",
                      [{"problem idx", ProblemIdx}]
                     ).

%% @doc Notify GUIs: The state of the current problem has been modified.
%% Send the new problem state to all GUIs.
-spec problem_state_changed(string()) -> ok.
problem_state_changed(NewState) ->
    send_event_to_gui("problem state changed",
                      [{"problem state", list_to_binary(NewState)}]
                     ).

%% @doc Notify GUIs of success/failure of a "save game" operation.
-spec ack_save_game_state(ok | file:posix()) -> ok.
ack_save_game_state(Result) ->
    send_event_to_gui("save game state",
                      [{"result", utils:atom_to_binary(Result)}]
                     ).

%% @doc Notify GUIs of success/failure of a "load game" operation.
-spec ack_load_game_state(atom()) -> ok.
ack_load_game_state(Result) ->
    send_event_to_gui("load game state",
                      [{"result", utils:atom_to_binary(Result)}]
                     ).

%% @doc Send all the data that the GUIs are allowed to know.
%% This can be used to (re)initialize a GUI.
-spec all_data(Running        :: boolean(),
               WorkerDataList :: [Worker],
               ProblemList    :: [Problem],
               ProblemIdx     :: non_neg_integer(),
               RoundNumber    :: non_neg_integer(),
               State          :: string(),
               WorkerInput    :: [string()]) -> ok when

      Worker  :: {ID           :: worker_id(),
                  Name         :: string(),
                  RankingGroup :: string(),
                  Proposition  :: string(),
                  PropCaption  :: string(),
                  PropScore    :: integer(),
                  ProblemScore :: integer(),
                  Blocked      :: no | {idx, non_neg_integer()},
                  Working      :: boolean()},

      Problem :: {string(), string(), pos_integer(), string()}.

all_data(Running, WorkerDataList, ProblemList, ProblemIdx, RoundNumber, State, WorkerInput) ->

    %% convert values to JSON-convertible types
    ProblemListBin = lists:map(fun({Description, Specification, AnswerTime, StartState}) ->
                                       [
                                        list_to_binary(Description),
                                        list_to_binary(Specification),
                                        AnswerTime,
                                        list_to_binary(StartState)
                                       ]
                               end,
                               ProblemList),
    WorkerDataListBin = lists:map(fun({ID, Name, RankingGroup, Proposition, PropCaption, Score,
                                       ProcessedScore, ProblemScore, Blocked, Working}) ->
                                          JSONProp = case Proposition of
                                                         none -> null;
                                                         PropString ->  list_to_binary(PropString)
                                                     end,
                                          JSONBlock = case Blocked of
                                                          no -> utils:atom_to_binary(no);
                                                          {idx, Idx} -> [{<<"id">>, Idx}]
                                                      end,
                                          [
                                           utils:atom_to_binary(ID),
                                           list_to_binary(Name),
                                           list_to_binary(RankingGroup),
                                           JSONProp,
                                           list_to_binary(PropCaption),
                                           Score,
                                           ProcessedScore,
                                           ProblemScore,
                                           JSONBlock,
                                           Working
                                          ]
                                  end,
                                  WorkerDataList),

    send_event_to_gui("all data",
                      [
                       {"running",      Running},
                       {"workers",      WorkerDataListBin},
                       {"problems",     ProblemListBin},
                       {"problem idx",  ProblemIdx},
                       {"round",        RoundNumber},
                       {"worker input", lists:map(fun list_to_binary/1, WorkerInput)},
                       {"state",        list_to_binary(State)}
                      ]
                     ).


%% ===================================================================
%% gui_port callbacks
%% ===================================================================

%% @doc Dispatch handlers according to contents of the received json message
-spec handle_json([{binary(), any()}]) -> ok | unknown_json.
handle_json(JsonTerm) ->
    case proplists:get_value(<<"action">>, JsonTerm) of
        <<"block worker">> ->
            json:process_attrs(fun dj:block_worker/1,
                               [{<<"worker id">>, fun utils:binary_to_atom/1}],
                               JsonTerm);
        <<"unblock worker">> ->
            json:process_attrs(fun dj:unblock_worker/1,
                               [{<<"worker id">>, fun utils:binary_to_atom/1}],
                               JsonTerm);
        <<"choose problem">> ->
            json:process_attrs(fun dj:choose_problem/1,
                               [<<"problem idx">>],
                               JsonTerm);
        <<"start round">> ->
            dj:start_round(),
            ok;
        <<"kill all workers">> ->
            dj:kill_all_workers(),
            ok;
        <<"apply proposition">> ->
            json:process_attrs(fun dj:apply_proposition/1,
                               [{<<"worker id">>, fun utils:binary_to_atom/1}],
                               JsonTerm);
        <<"load game state">> ->
            json:process_attrs(fun dj:load_game_state/1,
                               [{<<"file path">>, fun erlang:binary_to_list/1}],
                               JsonTerm);
        <<"save game state">> ->
            json:process_attrs(fun dj:save_game_state/1,
                               [{<<"file path">>, fun erlang:binary_to_list/1}],
                               JsonTerm);
        <<"add scores">> ->
            dj:add_scores(),
            ok;
        <<"quit program">> ->
            dj:quit_program(),
            ok;
        <<"get all data">> ->
            get_all_data(),
            ok;
        _ ->
            unknown_json
    end.


%% @doc Passthrough function to request all data from dj.
%% This makes sense so the {@link gui_port} doesn't have to call the
%% dj directly to initialize the GUI.
-spec get_all_data() -> ok.
get_all_data() ->
    dj:gui_wants_all_data(),
    ok.


%% ===================================================================
%% private functions
%% ===================================================================

-spec send_event_to_gui(string(), list({string(), term()})) -> ok.
send_event_to_gui(Name, Attributes) ->
    {ok, GuiProgList} = application:get_env(gui),
    BinaryfiedAttrs = lists:map(fun({Key, Val}) -> {list_to_binary(Key), Val} end, Attributes),
    Term = [{<<"event">>, list_to_binary(Name)}|BinaryfiedAttrs],
    lists:foldl(fun(_ExtProg, Idx) ->
                        gui_port:send(Idx, json:to_json_msg(Term)),
                        Idx + 1
                end, 0, GuiProgList),
    ok.

%% ===================================================================
