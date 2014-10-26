%% -------------------------------------------------------------------
%% @doc Handles interaction with the validator component.<br/>
%% The validator is responsible for verifying and scoring propositions
%% from the workers. This is the main validator module, the rest of
%% the application should use this to interface with the validator
%% component.<br/>
%% The function {@link start_link} starts a single
%% {@link validator_sup} which then starts a proposition queuing
%% process from {@link validator_queue} and a port process
%% ({@link validator_port}) via another supervisor
%% {@link validator_port_sup}).
%% @end
%% -------------------------------------------------------------------

-module(validator).

%% application programming interface
-export([
         start_link/1,
         validate/3,
         round_started/0,
         all_workers_stopped/0
        ]).


%% ===================================================================
%% application programming interface
%% ===================================================================

%% @doc Starts the whole validator infrastructure, including queue and
%% external executable.
start_link(ExtProg) ->
    validator_sup:start_link(ExtProg).

%% @doc Submits a single proposition from a worker to the validator.
-spec validate(atom(), [string()], string()) -> ok.
validate(WorkerID, WorkerInput, WorkerOutput) ->
    validator_queue:insert_proposition(WorkerID, WorkerInput, WorkerOutput).

%% @doc Informs the validator that the workers are working on a new
%% round.<br/>
%% The validator has to be ware of this state because when the workers
%% are stopped and the queue runs dry, the validator will tell the dj
%% that it's OK to end the round. This shouldn't happen while the
%% workers are running.
-spec round_started() -> ok.
round_started() ->
    validator_queue:round_started().

%% @doc Informs the validator that all workers are stopped.<br/>
%% The next time the queue runs dry, the validator assumes that all
%% propositions have been processed and tells the dj to end the round.
-spec all_workers_stopped() -> ok.
all_workers_stopped() ->
    validator_queue:all_workers_stopped().



%% ===================================================================
