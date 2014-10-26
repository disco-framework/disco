-module(worker_app).

-behavior(application).

%% called by boot script
-export([
         start/0,
         get_worker_name/2,
         give_worker_challenge/4,
         kill_worker/1
        ]).

%% called by application module
-export([
         start/2,
         stop/1
        ]).

%% called by port owner / worker server
-export([
         submit_proposition/2,
         worker_stopped/1,
         block_worker/1
        ]).

%% ===================================================================
%% application programming interface
%% ===================================================================

%% application start point
-spec start() -> 'ok' | {'error',_}.
start() ->
    %% sync global registries over all nodes
    case global:sync() of
        ok ->
            application:start(worker);

        {error, Reason} ->
            io:format("ERROR: global name server synchronization failed with reason ~p~n", [Reason])
    end.

-spec get_worker_name(atom(), non_neg_integer()) -> string().
get_worker_name(WorkerID, Timeout) ->
    case worker_server:get_name(WorkerID, Timeout) of
        ignored ->
            "";
        Name ->
            Name
    end.

-spec give_worker_challenge(atom(), [string()], non_neg_integer(), non_neg_integer()) -> ok | {error, any()}.
give_worker_challenge(WorkerID, Challenge, RoundTime, Timeout) ->
    case worker_server:give_challenge(WorkerID, Challenge, RoundTime, Timeout) of
        ignored ->
            ok;
        Answer ->
            Answer
    end.

-spec kill_worker(atom()) -> ok.
kill_worker(WorkerID) ->
    worker_server:kill(WorkerID).


%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = lagerd:debug("Application 'worker' started"),
    {ok, [[NodeName]]}     = init:get_argument(name),
    {ok, [[Master]]}       = init:get_argument(master),
    {ok, FileCwd}          = file:get_cwd(),
    {ok, WorkingDir}       = application:get_env(path_prefix),
    {ok, WorkerID}         = application:get_env(worker_id),
    {ok, WorkerRunCmdPat}  = application:get_env(worker_run_cmd),
    {ok, WorkerNameCmdPat} = application:get_env(worker_name_cmd),
    {ok, StartTestTimeout} = application:get_env(start_test_timeout),
    WorkerRunCmd = replace_all(WorkerRunCmdPat, "%id%", atom_to_list(WorkerID)),
    WorkerNameCmd = replace_all(WorkerNameCmdPat, "%id%", atom_to_list(WorkerID)),
    ok = lagerd:debug(lists:concat(["~nNode parameters:~n",
                                    "    | node name       : ~p~n",
                                    "    | master node     : ~p~n",
                                    "    | os:cmd(whoami)  = ~p~n",
                                    "    | os:cmd(pwd)     = ~p~n",
                                    "    | file:get_cwd()  = ~p~n",
                                    "    | port working dir: ~p~n",
                                    "    | worker id       : ~p~n",
                                    "    | worker run cmd  : ~p~n",
                                    "    | worker name cmd : ~p~n"]),
                      [NodeName, Master, os:cmd(whoami), os:cmd(pwd), FileCwd,
                       WorkingDir, WorkerID, WorkerRunCmd, WorkerNameCmd]),
    ok = lagerd:debug("Testing worker ~p...", [WorkerID]),
    case port_utils:try_open(WorkingDir, WorkerRunCmd, StartTestTimeout) of
        ok ->
            ok = lagerd:debug("...worker ~p OK", [WorkerID]),
            worker_sup:start_link(WorkerID, WorkingDir, WorkerRunCmd, WorkerNameCmd, false);
        {error, Output} ->
            ok = lagerd:error("Startup failed:~n~s", [Output]),
            worker_sup:start_link(WorkerID, WorkingDir, WorkerRunCmd, WorkerNameCmd, true)
    end.

stop(_State) ->
    ok.


%% ===================================================================
%% port owner / worker server callbacks
%% ===================================================================

-spec submit_proposition(atom(), string()) -> ok.
submit_proposition(WorkerID, Proposition) ->
    dj:submit_proposition(WorkerID, Proposition).

-spec worker_stopped(atom()) -> ok.
worker_stopped(WorkerID) ->
    dj:worker_stopped(WorkerID).

-spec block_worker(atom()) -> ok.
block_worker(WorkerID) ->
    dj:block_worker(WorkerID).


%% ===================================================================
%% private functions
%% ===================================================================

-spec replace_all(string(), string(), string()) -> string().
replace_all(Subject, Old, New) ->
    re:replace(Subject, Old, New, [global, {return, list}]).

%% ===================================================================
