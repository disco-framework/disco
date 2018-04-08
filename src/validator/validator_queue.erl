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

-behavior(gen_statem).

-include("validator_data.hrl").

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

%% called by gen_statem module
-export([
         init/1,
         terminate/3,
         code_change/4,
         callback_mode/0,
         %% custom state names
         empty/3,
         non_empty/3
        ]).

%% name under which to register the process
-define(REG_NAME, validator_queue).

-record(state, {queue             = queue:new() :: queue:queue(proposition()),
                workers_working   = false       :: boolean(),
                waiting_validator = none        :: {pid(), term()} | none}).

%% ===================================================================
%% application programming interface
%% ===================================================================

-spec start_link() -> ignore | {error, _} | {ok, pid()}.
start_link() ->
    gen_statem:start_link({local, ?REG_NAME}, ?MODULE, [], []).

-spec insert_proposition(worker_id(), [string()], string()) -> ok.
insert_proposition(WorkerID, WorkerInput, WorkerOutput) ->
    Proposition = {WorkerID, WorkerInput, WorkerOutput},
    gen_statem:cast(?REG_NAME, {insert, Proposition}).

%% @doc For debugging: Returns the content of the queue
get_queue_content() ->
    gen_statem:call(?REG_NAME, get_queue_content).

-spec round_started() -> ok.
round_started() ->
    gen_statem:cast(?REG_NAME, round_started).

-spec all_workers_stopped() -> ok.
all_workers_stopped() ->
    gen_statem:cast(?REG_NAME, all_workers_stopped).

%% ===================================================================
%% application programming interface for validator port
%% ===================================================================

%% @doc Fetches a proposition, blocking the calling thread if queue is empty.
-spec get_proposition() -> proposition().
get_proposition() ->
    gen_statem:call(?REG_NAME, get_proposition, infinity).

%% @doc Returns a validation result.
-spec put_score(proposition(), non_neg_integer(), string()) -> ok.
put_score(Proposition, Score, Caption) ->
    gen_statem:cast(?REG_NAME, {put_score, Proposition, Score, Caption}).


%% ===================================================================
%% gen_statem callbacks
%% ===================================================================

callback_mode() ->
    state_functions.

init([]) ->
    {ok, empty, #state{}}.

%% -- state callbacks --

empty(cast, all_workers_stopped, Data) ->
    dj:validator_queue_empty(),
    {keep_state, Data#state{workers_working=false}};

empty(cast, {insert, Prop}, Data=#state{queue=Queue,
                                                waiting_validator=WaitingValidator}) ->
    NewQueue = queue:in(Prop, Queue),
    case WaitingValidator of
        none ->
            {next_state, non_empty, Data#state{queue=NewQueue}};
        _FromSpec ->
            gen_statem:reply(WaitingValidator, Prop),
            {next_state, non_empty, Data#state{waiting_validator=none,
                                               queue=NewQueue}}
    end;

empty({call, From}, get_proposition, Data=#state{waiting_validator=WaitingValidator}) ->
    %% If a waiting validator crashes, there will be a new validator process
    %% asking for a proposition. In this case, the old process ID will be overwritten.
    case WaitingValidator of
        none ->
            ok;
        _FromSpec ->
            ok = lager:warning("Validator queue received get_proposition from ~p, overriding the already waiting validator ~p", [From, WaitingValidator])
    end,
    %% reply later, when there are elements in the queue (blocking call)
    {keep_state, Data#state{waiting_validator=From}};

empty(Type, Msg, Data) ->
    handle_event(Type, Msg, empty, Data).

%% -----

non_empty(cast, {put_score, Prop, Score, Caption}, Data=#state{queue=Queue, workers_working=WorkersWorking}) ->
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
                    {next_state, empty, Data#state{queue=NewQueue}};
                false ->
                    {keep_state, Data#state{queue=NewQueue}}
            end;
        {{value, _}, _} ->
            ok = lager:warning("Validator queue received score for unexpected proposition: ~p~nQueue content: ~p",
                          [Prop, queue:to_list(Queue)]),
            {keep_state, Data#state{queue=Queue}};
        {empty, _} ->
            ok = lager:critical("Validator queue is empty in nonempty state!"),
            {next_state, empty, Data#state{queue=Queue}}
    end;

non_empty(cast, all_workers_stopped, Data) ->
    {keep_state, Data#state{workers_working=false}};

non_empty(cast, {insert, Prop}, Data=#state{queue=Queue,
                                            waiting_validator=WaitingValidator}) ->
    NewQueue = queue:in(Prop, Queue),
    case WaitingValidator of
        none ->
            {keep_state, Data#state{queue=NewQueue}};
        _FromSpec ->
            ok = lager:critical("There should not be a waiting validator when the queue is not empty!"),
            throw(validator_queue_corrupted)
    end;

non_empty({call, From}, get_proposition, Data=#state{queue=Queue}) ->
    case queue:out(Queue) of
        {{value, Prop}, _} ->
            {keep_state_and_data, {reply, From, Prop}};
        {empty, _} ->
            ok = lager:critical("Validator queue is empty in nonempty state!"),
            {next_state, empty, Data, {reply, From, queue_error}}
    end;

non_empty(Type, Msg, Data) ->
    handle_event(Type, Msg, non_empty, Data).


%% -----

code_change(_OldVsn, DataName, Data, _Extra) ->
    %% No change planned.
    %% The function is there for the behaviour, but will not be used.
    {ok, DataName, Data}.

terminate(_Msg, _StateName, _Data) ->
    ok.


%% ===================================================================
%% private functions
%% ===================================================================

%% -- stateless callbacks --

handle_event(cast, round_started, _State, Data) ->
    {keep_state, Data#state{workers_working=true}};
handle_event({call, From}, get_queue_content, _State, Data=#state{queue=Queue}) ->
    {keep_state, Data, [{reply, From, queue:to_list(Queue)}]};
handle_event(Type, Msg, State, Data) ->
    ok = lager:error("Validator queue received unknown event ~p from ~p while in state ~p",
                [Msg, Type, State]),
    {keep_state, Data}.


%% ===================================================================
