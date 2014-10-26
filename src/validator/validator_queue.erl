%% -------------------------------------------------------------------
%% @doc Provides a process to queue incoming propositions.<br/>
%% The validator pulls the propositions from the queue via
%% {@link get_proposition/0} and returns the answer via
%% {@link put_score/3}.
%% The queue process keeps proposition until it gets the validation
%% result so when the validator crashes, no proposition is lost.<br/>
%% When a round is over, there may still be unvalidated propositions
%% in the queue. Therefore, the {@link dj} tells the queue that the
%% round time is over ({@link all_workers_stopped/0}) and the queue
%% sends a message back the next time it runs dry. When this has
%% happened, the final results for the round can be computed and the
%% round is officially over.<br/>
%% In this text, "validator" means a process spawned by the
%% {@link validator_port} module.
%% @end
%% -------------------------------------------------------------------

-module(validator_queue).

-behavior(gen_fsm).

-include("validator_data.hrl").
-include("../global_types.hrl").

%% application programming interface
-export([
         start_link/0,
         insert_proposition/3,
         get_queue_content/0,
         round_started/0,
         all_workers_stopped/0
        ]).

%% application programming interface for validator port
-export([
         get_proposition/0,
         put_score/3
        ]).

%% called by gen_fsm module
-export([
         init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4,
         %% custom state names
         empty/2,
         empty/3,
         non_empty/2,
         non_empty/3
        ]).

%% name under which to register the process
-define(REG_NAME, validator_queue).

-record(state, {queue             = queue:new() :: queue(),
                workers_working   = false       :: boolean(),
                waiting_validator = none        :: {pid(), term()} | none}).

%% ===================================================================
%% application programming interface
%% ===================================================================

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
start_link() ->
    gen_fsm:start_link({local, ?REG_NAME}, ?MODULE, [], []).

-spec insert_proposition(worker_id(), [string()], string()) -> ok.
insert_proposition(WorkerID, WorkerInput, WorkerOutput) ->
    Proposition = {WorkerID, WorkerInput, WorkerOutput},
    gen_fsm:send_all_state_event(?REG_NAME, {insert, Proposition}).

%% @doc For debugging: Returns the content of the queue
get_queue_content() ->
    gen_fsm:sync_send_all_state_event(?REG_NAME, get_queue_content).

-spec round_started() -> ok.
round_started() ->
    gen_fsm:send_all_state_event(?REG_NAME, round_started).

-spec all_workers_stopped() -> ok.
all_workers_stopped() ->
    gen_fsm:send_event(?REG_NAME, all_workers_stopped).

%% ===================================================================
%% application programming interface for validator port
%% ===================================================================

%% @doc Fetches a proposition, blocking the calling thread if queue is empty.
-spec get_proposition() -> proposition().
get_proposition() ->
    gen_fsm:sync_send_event(?REG_NAME, get_proposition, infinity).

%% @doc Returns a validation result.
-spec put_score(proposition(), non_neg_integer(), string()) -> ok.
put_score(Proposition, Score, Caption) ->
    gen_fsm:send_event(?REG_NAME, {put_score, Proposition, Score, Caption}).


%% ===================================================================
%% gen_fsm callbacks
%% ===================================================================


init([]) ->
    {ok, empty, #state{}}.

%% -- state callbacks --

empty(all_workers_stopped, Data) ->
    dj:validator_queue_empty(),
    {next_state, empty, Data#state{workers_working=false}};

empty(Msg, Data) ->
    unexpected(Msg, empty),
    {reply, unexpected, empty, Data}.

empty(get_proposition, From, Data=#state{waiting_validator=WaitingValidator}) ->
    %% If a waiting validator crashes, there will be a new validator process
    %% asking for a proposition. In this case, the old process ID will be overwritten.
    case WaitingValidator of
        none ->
            ok;
        _FromSpec ->
            ok = lager:warning("Validator queue received get_proposition from ~p, overriding the already waiting validator ~p", [From, WaitingValidator])
    end,
    %% reply later, when there are elements in the queue (blocking call)
    {next_state, empty, Data#state{waiting_validator=From}};

empty(Msg, _From, Data) ->
    unexpected(Msg, empty),
    {reply, unexpected, empty, Data}.

%% -----

non_empty({put_score, Prop, Score, Caption}, Data=#state{queue=Queue, workers_working=WorkersWorking}) ->
    case queue:out(Queue) of
        {{value, Prop}, NewQueue} ->
            {WorkerID, WorkerInput, WorkerOutput} = Prop,
            dj:submit_validated_proposition(WorkerID, WorkerInput, WorkerOutput, Score, Caption),
            ok = lager:debug("Validator queue received validated proposition: ~p => score: ~p, caption: ~p",
                        [Prop, Score, Caption]),
            case queue:is_empty(NewQueue) of
                true  ->
                    case WorkersWorking of
                        false -> dj:validator_queue_empty();
                        _ -> ok
                    end,
                    {next_state, empty,     Data#state{queue=NewQueue}};
                false ->
                    {next_state, non_empty, Data#state{queue=NewQueue}}
            end;
        {{value, _}, _} ->
            ok = lager:warning("Validator queue received score for unexpected proposition: ~p~nQueue content: ~p",
                          [Prop, queue:to_list(Queue)]),
            {next_state, non_empty, Data#state{queue=Queue}};
        {empty, _} ->
            ok = lager:critical("Validator queue is empty in nonempty state!"),
            {next_state, empty, Data#state{queue=Queue}}
    end;

non_empty(all_workers_stopped, Data) ->
    {next_state, non_empty, Data#state{workers_working=false}};

non_empty(Event, Data) ->
    unexpected(Event, non_empty),
    {next_state, non_empty, Data}.


non_empty(get_proposition, _From, Data=#state{queue=Queue}) ->
    case queue:out(Queue) of
        {{value, Prop}, _} ->
            {reply, Prop, non_empty, Data};
        {empty, _} ->
            ok = lager:critical("Validator queue is empty in nonempty state!"),
            {reply, queue_error, empty, Data}
    end;

non_empty(Event, _From, Data) ->
    unexpected(Event, non_empty),
    {reply, unexpected, non_empty, Data}.


%% -- stateless callbacks --

handle_event({insert, Prop}, State, Data=#state{queue=Queue,
                                                waiting_validator=WaitingValidator}) ->
    NewQueue = queue:in(Prop, Queue),
    case WaitingValidator of
        none ->
            {next_state, non_empty, Data#state{queue=NewQueue}};
        _FromSpec ->
            case State of
                non_empty ->
                    ok = lager:critical("There should not be a waiting validator when the queue is not empty!"),
                    throw(validator_queue_corrupted);
                empty ->
                    gen_fsm:reply(WaitingValidator, Prop),
                    {next_state, non_empty, Data#state{waiting_validator=none,
                                                       queue=NewQueue}}
            end
    end;
handle_event(round_started, State, Data) ->
    {next_state, State, Data#state{workers_working=true}};
handle_event(Event, State, Data) ->
    unexpected(Event, State),
    {next_state, State, Data}.


handle_sync_event(get_queue_content, _From, State, Data=#state{queue=Queue}) ->
    {reply, queue:to_list(Queue), State, Data};
handle_sync_event(Event, _From, State, Data) ->
    unexpected(Event, State),
    {reply, unexpected, State, Data}.


%% -----

code_change(_OldVsn, DataName, Data, _Extra) ->
    %% No change planned.
    %% The function is there for the behaviour, but will not be used.
    {ok, DataName, Data}.

terminate(_Msg, _StateName, _Data) ->
    ok.

handle_info(Info, State, Data) ->
    unexpected(Info, State),
    {next_state, State, Data}.


%% ===================================================================
%% private functions
%% ===================================================================

%% Unexpected allows to log unexpected messages
-spec unexpected(term(), atom()) -> any().
unexpected(Msg, State) ->
    ok = lager:error("Validator queue received unknown event ~p while in state ~p",
                [Msg, State]).

%% ===================================================================
