-module(worker_sup).

-behaviour(supervisor).

%% application programming interface
-export([start_link/5]).

%% supervisor callbacks
-export([init/1]).


%% ===================================================================
%% application programming interface
%% ===================================================================

-spec start_link(atom(), string(), string(), string(), boolean()) -> any().
start_link(WorkerID, WorkingDir, RunCmd, NameCmd, IgnoreMode) ->
    supervisor:start_link(?MODULE, [WorkerID, WorkingDir, RunCmd, NameCmd, IgnoreMode]).


%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([WorkerID, WorkingDir, WorkerRunCmd, WorkerNameCmd, IgnoreMode]) ->
    RestartStrategy = rest_for_one,        %% one_for_one | one_for_all | rest_for_one
    MaxRestart      = 20,                  %% max number of restarts in the last ...
    MaxTime         = 1,                   %% ... seconds
    ServerChild =
        {worker_server_child,           %% child id
         {worker_server, start_link,    %% startfunc: {Module, Funktion, Args}
          [WorkerID, WorkingDir, WorkerNameCmd, IgnoreMode]},
         permanent,                     %% restart  : permanent | temporary | transient
         10,                            %% shutdown timeout in ms
         worker,                        %% type     : worker | supervisor
         [worker_server]                %% modules  : [Module] | dynamic
        },
    PortOwnerChild =
           {worker_port_owner_child,       %% child id
            {worker_port_owner, start_link,%% startfunc: {Module, Funktion, Args}
             [WorkerID, WorkingDir, WorkerRunCmd]},
            permanent,                     %% restart  : permanent | temporary | transient
            10,                            %% shutdown timeout in ms
            worker,                        %% type     : worker | supervisor
            [worker_port_owner]            %% modules  : [Module] | dynamic
           },
    case IgnoreMode of
        false ->
            {ok, {{RestartStrategy, MaxRestart, MaxTime},
                  [ServerChild, PortOwnerChild]
                 }
            };
        true ->
            {ok, {{RestartStrategy, MaxRestart, MaxTime},
                  [ServerChild]
                 }
            }
    end.


%% ===================================================================
