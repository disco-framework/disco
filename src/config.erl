%% -------------------------------------------------------------------
%% @doc Validates the configuration.
%% Checks the application's environment for all necessary values
%% and their ranges.
%% @end
%% -------------------------------------------------------------------

-module(config).

-export([
         check/0,
         check_verbose/0,
         valid_score_mode/1
        ]).

-include("global_types.hrl").

%% @doc Check the configuration values in the application environment
%% and just return whether they're complete and valid.
-spec check() -> error | ok.
check() ->
    case check_verbose() of
        [] ->
            ok;
        ErrorList ->
            ok = lager:error(utils:intersperse("\n - ", [ "Errors in configuration:" | ErrorList ])),
            error
    end.

%% @doc Check the configuration values in the application environment
%% and return verbose error messages.
-spec check_verbose() -> [string()].
check_verbose() ->
    ResultList =
        [
         check_config_string(worker_run_cmd, "Workers run command"),

         check_config_string(worker_name_cmd, "Workers name command"),

         case application:get_env(workers) of
             {ok, []} ->
                 {error, "Worker list is empty"};
             {ok, WorkerSpecs} when is_list(WorkerSpecs) ->
                 case [S || S <- WorkerSpecs, not valid_worker_entry(S)] of
                     [] ->
                         case valid_unique_worker_ids(WorkerSpecs) of
                             true ->
                                 ok;
                             false ->
                                 {error, "Worker list contains duplicate IDs"}
                         end;
                     InvalidEntries ->
                         {error, sformat("Worker list contains invalid entries: ~p", [InvalidEntries])}
                 end;
             {ok, WorkerSpecs} ->
                 {error, sformat("Worker list has invalid format: ~p", [WorkerSpecs])};
             undefined ->
                 {error, "Worker list undefined"}
         end,

         case application:get_env(problems) of
             {ok, []} ->
                 {error, "Problem list is empty"};
             {ok, Problems=[_FirstProblem|_]}->
                 case [P || P <- Problems, not valid_problem_entry(P)] of
                     [] ->
                         ok;
                     InvalidEntries ->
                         {error, sformat("Problem list contains invalid entries: ~p", [InvalidEntries])}
                 end;
             {ok, Problems} ->
                 {error, sformat("Worker list has invalid format: ~p", [Problems])};
             undefined ->
                 {error, "Problem list undefined"}
         end,

         check_config_string(validator, "Validator command"),

         check_config_string(barkeeper, "Barkeeper command"),

         check_config_string(changer, "Changer command"),

         case application:get_env(score_mode) of
             {ok, Val} ->
                 case valid_score_mode(Val) of
                     true ->
                         ok;
                     false ->
                         {error, sformat("invalid score mode: ~p", [Val])}
                 end;
             undefined ->
                 {error, "Score mode undefined"}
         end,

         case application:get_env(gui) of
             {ok, Val} ->
                 case is_list(Val) andalso lists:all(fun valid_string/1, Val) of
                     true ->
                         ok;
                     false ->
                         {error, sformat("GUI command list has invalid format: ~p", [Val])}
                 end;
             undefined ->
                 {error, "GUI command list undefined"}
         end,

         check_config_string(remote_node_path_prefix, "Remote node path prefix"),

         check_config_pos_integer(worker_call_timeout, "Worker call timeout"),

         check_config_pos_integer(port_call_timeout, "Port call timeout"),

         check_config_pos_integer(port_buffer_size, "Port buffer size"),

         case application:get_env(autosave_dir) of
             {ok, disabled} ->
                 ok;
             _ ->
                 check_config_string(autosave_dir, "Autosave dir")
         end,

         case application:get_env(cluster_start_mode) of
             {ok, sequential} ->
                 ok;
             {ok, parallel} ->
                 ok;
             {ok, Val} ->
                 {error, sformat("Cluster start mode has invalid format: ~p", [Val])};
             undefined ->
                 {error, "Cluster start mode undefined"}
         end,

         check_config_pos_integer(startup_slice_size, "Parallel startup slice size"),

         check_config_pos_integer(component_test_timeout, "Component startup test timeout"),

         check_config_bool(do_component_test, "Component test flag")
        ],

    [ String || {error, String} <- ResultList ].


%% @doc Check that a value with the given key is present and a positive integer.
-spec check_config_pos_integer(atom(), string()) -> ok | {error, string()}.
check_config_pos_integer(Parameter, LogPrefix) ->
    case application:get_env(Parameter) of
        {ok, Val} when is_integer(Val), Val > 0 ->
            ok;
        {ok, Val} when is_integer(Val) ->
            {error, sformat("~s out of range: ~p", [LogPrefix, Val])};
        {ok, Val} ->
            {error, sformat("~s has invalid format: ~p", [LogPrefix, Val])};
        undefined ->
            {error, sformat("~s undefined", [LogPrefix])}
    end.

%% @doc Check that a value with the given key is present and a string.
-spec check_config_string(atom(), string()) -> ok | {error, string()}.
check_config_string(Key, LogPrefix) ->
    case application:get_env(Key) of
        {ok, Val} ->
            case valid_string(Val) of
                true ->
                    ok;
                false ->
                    {error, sformat("~s is not a string: ~p", [LogPrefix, Val])}
            end;
        undefined ->
            {error, sformat("~s undefined", [LogPrefix])}
    end.

%% @doc Check that a value with the given key is present and a boolean.
-spec check_config_bool(atom(), string()) -> ok | {error, string()}.
check_config_bool(Key, LogPrefix) ->
    case application:get_env(Key) of
        {ok, Val} ->
            case is_boolean(Val) of
                true ->
                    ok;
                false ->
                    {error, sformat("~s is not a boolean: ~p", [LogPrefix, Val])}
            end;
        undefined ->
            {error, sformat("~s undefined", [LogPrefix])}
    end.

-spec valid_score_mode(score_mode()) -> boolean().
valid_score_mode(Mode) ->
    lists:member(Mode, [raw, normalized, ranked]).

%% @doc Check that the given list of worker specifications does not contain
%% duplicate IDs.
-spec valid_unique_worker_ids([{atom(),_,_}]) -> boolean().
valid_unique_worker_ids(WorkerSpecs) ->
    UniqueList = lists:usort(fun({WorkerID1, _, _}, {WorkerID2, _, _}) ->
                                     WorkerID1 =< WorkerID2
                             end,
                             WorkerSpecs),
    length(UniqueList) == length(WorkerSpecs).

%% @doc Check that the given worker specification has the right type.
-spec valid_worker_entry(any()) -> boolean().
valid_worker_entry({WorkerID, IP, RankingGroup})
  when is_atom(WorkerID),
       is_atom(IP) ->
    valid_string(RankingGroup);
valid_worker_entry(_) ->
    false.

%% @doc Check that the given problem specification has the right type.
-spec valid_problem_entry(any()) -> boolean().
valid_problem_entry({Desc, Spec, AnswerTime, StartState})
  when is_integer(AnswerTime),
       AnswerTime > 0 ->
    valid_string(Desc) andalso valid_string(Spec) andalso valid_string(StartState);
valid_problem_entry(_) ->
    false.

%% @doc Check for real string (list of characters)
%% Unfortunately this is an expensive operation and can not be used in guards.
-spec valid_string(any()) -> boolean().
valid_string([]) ->
    true;
valid_string([X|T]) ->
    is_integer(X) andalso (X >= 0) andalso valid_string(T);
valid_string(_) ->
    false.

%% @doc simple sprintf-like string format function
-spec sformat(string(), [any()]) -> string() |
                             {error, [any()], any()} |
                             {incomplete, [any()], binary()}.
sformat(Format, Vars) ->
    unicode:characters_to_list(io_lib:format(Format, Vars)).
