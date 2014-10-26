-module(worker_server).

-behaviour(gen_server).

%% application programming interface
-export([
         start_link/4,
         get_name/2,
         give_challenge/4,
         kill/1
        ]).

%% called by worker_port_owner module
-export([
         set_port_ref/2
        ]).

%% called by gen_server module
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

%% called by timer
-export([
         close_port/2
         ]).

%% server state
-record(state, {worker_id       = ''        :: atom(),
                working_dir     = ""        :: string(),
                worker_name_cmd = ""        :: string(),
                port_ref        = undefined :: port() | undefined,
                ignore          = false     :: boolean(),
                kill_requesters = []        :: [pid()]}).

-define(REG_NAME(WorkerID), {global, utils:get_worker_reg_name(WorkerID)}).


%% ===================================================================
%% application programming interface
%% ===================================================================

start_link(WorkerID, WorkingDir, WorkerNameCmd, IgnoreMode) ->
    %% start gen_server and register
    gen_server:start_link(?REG_NAME(WorkerID), ?MODULE, [WorkerID, WorkingDir, WorkerNameCmd, IgnoreMode], []).

-spec get_name(atom(), non_neg_integer()) -> string() | ignored.
get_name(WorkerID, Timeout) ->
    gen_server:call(?REG_NAME(WorkerID), get_name, Timeout).

-spec give_challenge(atom(), [string()], non_neg_integer(), non_neg_integer()) -> ok | {error, any()} | ignored.
give_challenge(WorkerID, Challenge, RoundTime, Timeout) ->
    gen_server:call(?REG_NAME(WorkerID), {give_challenge, Challenge, RoundTime}, Timeout).

-spec kill(atom()) -> ok.
kill(WorkerID) ->
    gen_server:cast(?REG_NAME(WorkerID), {kill, self()}),
    receive
        {ok, restarted} ->
            ok
    end.


%% ===================================================================
%% port owner callbacks
%% ===================================================================

set_port_ref(WorkerID, Port) ->
    gen_server:call(?REG_NAME(WorkerID), {set_port_ref, Port}, 100).


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([WorkerID, WorkingDir, WorkerNameCmd, IgnoreMode]) ->
    {ok, #state{worker_id=WorkerID,
                working_dir=WorkingDir,
                worker_name_cmd=WorkerNameCmd,
                ignore=IgnoreMode}}.

%% ----------

handle_call({set_port_ref, Port}, _From, State=#state{ignore=false,kill_requesters=KillRequesters}) ->
    lists:foreach(fun(Pid) ->
                          Pid ! {ok, restarted}
                  end,
                  KillRequesters),
    {reply, ok, State#state{port_ref=Port, kill_requesters=[]}};

handle_call(get_name, _From, State = #state{working_dir=WorkingDir, worker_name_cmd=WorkerNameCmd, ignore=false}) ->
    Cmd = case length(WorkingDir) > 0 of
              true -> "cd " ++ WorkingDir ++ "; " ++ WorkerNameCmd;
              false -> WorkerNameCmd
          end,
    WorkerName = os:cmd(Cmd),
    {reply, string:strip(WorkerName, right, $\n), State};

handle_call({give_challenge, Challenge, RoundTime}, _From, State = #state{worker_id=WorkerID, port_ref=Port, ignore=false}) ->
    case Port of
        undefined ->
            {stop, no_port_ref};

        _ ->
            WorkerInput = make_workerinput_messages(Challenge),
            case send_to_port(Port, WorkerInput) of
                ok ->
                    {Res, _} = timer:apply_after(RoundTime, ?MODULE, close_port, [Port, WorkerID]),
                    {reply, Res, State};
                Error ->
                    {reply, {error, Error}, State}
            end
    end;

handle_call(Msg, _From, State=#state{ignore=true, worker_id=WorkerID}) ->
    lagerd:debug("Ignored call: ~p", [Msg]),
    worker_app:block_worker(WorkerID),
    {reply, ignored, State}.

%% ----------

handle_cast({kill, Requester}, State = #state{worker_id=WorkerID,
                                              port_ref=Port,
                                              ignore=false,
                                              kill_requesters=Requesters}) ->
    close_port(Port, WorkerID),
    {noreply, State#state{kill_requesters=[Requester|Requesters]}};

handle_cast({kill, Requester}, State=#state{ignore=true, worker_id=WorkerID}) ->
    lagerd:debug("Ignored cast: kill"),
    %% do not block worker here to avoid infinite loop
    %% instead pretend everything worked
    worker_app:worker_stopped(WorkerID),
    Requester ! {ok, restarted},
    {noreply, State};

handle_cast(Msg, State=#state{ignore=true, worker_id=WorkerID}) ->
    lagerd:debug("Ignored cast: ~p", [Msg]),
    worker_app:block_worker(WorkerID),
    {noreply, State}.


%% ----------

handle_info(Info, State) ->
    lagerd:warning("unhandled info message: ~p", [Info]),
    {noreply, State}.

%% ----------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------

terminate(Reason, #state{worker_id=WorkerID}) ->
    lagerd:info("Worker server ~p terminated with reason ~p", [WorkerID, Reason]).


%% ===================================================================
%% private functions
%% ===================================================================

-spec make_workerinput_messages([string()]) -> nonempty_string().
make_workerinput_messages(Challenge) ->
    utils:join("\n", Challenge) ++ "\n".

-spec send_to_port(port(), nonempty_string()) -> ok | port_closed.
send_to_port(Port, Msg) ->
    case erlang:port_info(Port) of
        undefined ->
            port_closed;
        _ ->
            port_command(Port, Msg),
            ok
    end.

-spec close_port(port(), atom()) -> ok | already_closed.
close_port(Port, WorkerID) ->
    case erlang:port_info(Port) of
        undefined ->
            lagerd:debug("Worker ~p already closed!", [WorkerID]),
            already_closed;
        _ ->
            lagerd:debug("Worker ~p killed!", [WorkerID]),
            port_close(Port),
            ok
    end.

%% ===================================================================
