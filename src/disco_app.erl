%% -------------------------------------------------------------------
%% @doc Starts up the framework.
%% @end
%% -------------------------------------------------------------------

-module(disco_app).

-behaviour(application).

-include("global_types.hrl").

%% called by boot script
-export([start/0]).

%% called by application module
-export([start/2,
         stop/1]).

%% ===================================================================

start() ->
    case init:get_argument(cover_enabled) of
        {ok, [["true"]]} -> cover_utils:init();
        _                -> ok
    end,

    %% start logger
    lager:start(),

    %% start the main application
    ok = lager:info("*** starting disco: distributed contest framework ***"),
    application:start(disco, permanent).


%% ===================================================================
%% application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case config:check() of
        ok ->
            %% start logger daemon
            {ok, _} = lagerd_sup:start_link(),

            %% get node list from config
            {ok, WorkerSpecs} = application:get_env(workers),
            ok = lager:debug("WorkerSpecs = ~p", [WorkerSpecs]),

            ok = lager:info("Testing external components..."),
            {ok, TestTimeout} = application:get_env(component_test_timeout),
            {ok, BarkeeperProg} = application:get_env(barkeeper),
            {ok, ValidatorProg} = application:get_env(validator),
            {ok, ChangerProg} = application:get_env(changer),
            {ok, GuiProgList} = application:get_env(gui),
            {ok, DoStartupTest} = application:get_env(do_component_test),

            StartupResults =
                case DoStartupTest of
                    true ->
                        utils:pmap(fun(Prog) ->
                                       ok = lager:info("Testing ~s", [Prog]),
                                       port_utils:try_open("", Prog, TestTimeout)
                                   end,
                                   [BarkeeperProg, ValidatorProg, ChangerProg] ++ GuiProgList);
                    false ->
                        []
                end,
            StartupErrors = lists:filter(fun(Res) -> Res /= ok end, StartupResults),
            case length(StartupErrors) of
                L when L > 0 ->
                    ok = lager:error("Shutting down because component startup failed."),
                    lists:foreach(fun({error, Output}) ->
                                          ok = lager:error("Component test output received:~n~s", [Output])
                                  end,
                                  StartupErrors),
                    init:stop(),
                    {ok, self()};
                _ ->
                    ok = lager:info("...external components OK"),

                    %% start distributed nodes
                    ok = lager:info("Starting node cluster..."),
                    StartWorkerNodeRes = start_worker_nodes(WorkerSpecs),
                    NotStarted = [ {Node, IP, Reason} || {error, Node, IP, Reason} <- StartWorkerNodeRes ],
                    case NotStarted of
                        %% if node startup was successful, start components
                        [] ->
                            ok = lager:info("...node cluster ready"),

                            ok = lager:info("Starting barkeeper..."),
                            {ok, _} = barkeeper_sup:start_link(BarkeeperProg),
                            ok = lager:info("...barkeeper ready"),

                            ok = lager:info("Starting validator..."),
                            {ok, _} = validator:start_link(ValidatorProg),
                            ok = lager:info("...validator ready"),

                            ok = lager:info("Starting changer..."),
                            {ok, _} = changer_sup:start_link(ChangerProg),
                            ok = lager:info("...changer ready"),

                            ok = lager:info("Starting dj..."),
                            {ok, _} = dj_sup:start_link(),
                            ok = lager:info("...dj ready"),

                            ok = lager:info("Starting gui..."),
                            {ok, _} = gui:start_link(GuiProgList),
                            ok = lager:info("...gui ready"),

                            {ok, self()};

                        _ ->
                            FormattedLines = [ io_lib:format("    ~s@~s: ~p", [Node, IP, Reason])
                                               || {Node, IP, Reason} <- NotStarted ],
                            ok = lager:error("Error starting nodes: ~n~s", [utils:join("\n", FormattedLines)]),
                            {error, starting_node_cluster_failed}
                    end
            end;

        error ->
            ok = lager:error("Config invalid. Stopping program"),
            init:stop(),
            {ok, self()}
    end.

stop(_State) ->
    ok.

%% ===================================================================
%% private functions
%% ===================================================================

-spec start_worker_node(worker_spec())
                       -> {ok, node()} |
                          {error, NodeName :: atom(), IP :: atom(), Reason} when
      Reason :: timeout | no_rsh | {already_running, node()}.
start_worker_node(Spec) ->
    {WorkerID, IP, _RankingGroup} = Spec,
    NodeName = utils:get_worker_reg_name(WorkerID),
    NodeArgs = worker_start_args(WorkerID),
    ok = lager:info(" - Starting node ~s@~s", [NodeName, IP]),
    ok = lager:debug("   slave:start(~p, ~p, ~p).", [IP, NodeName, NodeArgs]),
    Res = slave:start(IP, NodeName, NodeArgs),
    ok = lager:debug("   slave:start(~p, ~p, ...) result: ~p", [IP, NodeName, Res]),
    case Res of
        {ok, _Node} ->
            Res;
        {error, Reason} ->
            {error, NodeName, IP, Reason}
    end.


-spec start_worker_nodes([worker_spec()]) -> [{ok, atom()} | {error, atom(), atom(), term()}].
start_worker_nodes(WorkerSpecs) ->
    {ok, StartMode} = application:get_env(cluster_start_mode),
    case StartMode of
        sequential ->
            Res = [ start_worker_node(Spec) || Spec <- WorkerSpecs ],
            wait_for_workers(WorkerSpecs),
            Res;
        parallel ->
            {ok, SliceSize} = application:get_env(startup_slice_size),
            lists:flatten(
              lists:map(
                fun(SpecSlice) ->
                        List = utils:pmap(fun start_worker_node/1, SpecSlice),
                        wait_for_workers(SpecSlice),
                        List
                end,
                utils:slice(SliceSize, WorkerSpecs)))
    end.

%% @doc Build the argument string for the worker nodes
worker_start_args(WorkerID)->
    {ok, PathPrefix}       = application:get_env(remote_node_path_prefix),
    {ok, WorkerRunCmd}     = application:get_env(worker_run_cmd),
    {ok, WorkerNameCmd}    = application:get_env(worker_name_cmd),
    {ok, StartTestTimeout} = application:get_env(worker_test_timeout),

    CookieArgs = case utils:get_argument(setcookie) of
                     {ok, [[Cookie]]} -> " -setcookie " ++ Cookie;
                     _                -> ""
                 end,
    PaArgs = case utils:get_argument(pa) of
                 {ok, RelPath} -> AbsPath = [ PathPrefix ++ Path || Path <- lists:concat(RelPath) ],
                                  " -pa " ++ utils:join(" ", lists:reverse(AbsPath));
                 _             -> ""
             end,
    SlaveArgs = " -s worker_app" ++
        " -worker" ++
        " path_prefix " ++ lists:flatten(io_lib:write(PathPrefix)) ++
        " worker_id " ++ lists:flatten(io_lib:write(WorkerID)) ++
        " worker_run_cmd " ++ lists:flatten(io_lib:write(WorkerRunCmd)) ++
        " worker_name_cmd " ++ lists:flatten(io_lib:write(WorkerNameCmd)) ++
        " start_test_timeout " ++ lists:flatten(io_lib:write(StartTestTimeout)),
    lists:concat([CookieArgs, PaArgs, SlaveArgs]).

%% @doc Wait until all workers are globally registered
-spec wait_for_workers([worker_spec()]) -> ok.
wait_for_workers(WorkerSpecs) ->
    WorkerRegNames = [ utils:get_worker_reg_name(WorkerID) || {WorkerID, _IP, _RankingGroup} <- WorkerSpecs ],
    {ok, StartTimeout} = application:get_env(node_start_timeout),
    ok = lager:debug("waiting for workers to register themselves: ~p", [WorkerRegNames]),
    wait_for_workers_loop(WorkerRegNames, StartTimeout),
    ok = lager:debug("...workers ~p globally registered", [WorkerRegNames]),
    ok.

-spec wait_for_workers_loop([atom()], timeout()) -> ok.
wait_for_workers_loop(WorkerRegNames, Timeout) ->
    WaitTime = 100, % polling period [ms]
    Missing = WorkerRegNames -- global:registered_names(), % -- is the list subtraction operator
    case Missing of
        [] -> ok;
        _  -> case Timeout of
                  infinity     -> utils:wait(WaitTime),
                                  wait_for_workers_loop(WorkerRegNames, infinity);
                  T when T > 0 -> utils:wait(WaitTime),
                                  wait_for_workers_loop(WorkerRegNames, Timeout - WaitTime);
                  _            -> ok = lager:error("Startup timed out. The following ~p worker node(s) did not respond: ~p",
                                                   [length(Missing), Missing]),
                                  init:stop()
              end
    end.


%% ===================================================================
