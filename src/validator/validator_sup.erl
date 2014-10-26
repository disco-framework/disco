-module(validator_sup).

-behaviour(supervisor).

%% application programming interface
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).


%% ===================================================================
%% application programming interface
%% ===================================================================

-spec start_link(string()) -> any().
start_link(ExtProg) ->
    supervisor:start_link(?MODULE, [ExtProg]).


%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init([ExtProg]) ->
    RestartStrategy = one_for_all,   % one_for_one | one_for_all | rest_for_one
    MaxRestart      = 1,             % max number of restarts in the last ...
    MaxTime         = 10,            % ... seconds
    QueueChild =
        { queue_child,                       % child id
          {validator_queue, start_link, []}, % startfunc: {Module, Funktion, Args}
          permanent,                         % restart  : permanent | temporary | transient
          300,                               % shutdown timeout in ms
          worker,                            % type     : worker | supervisor
          [validator_queue]                  % modules  : [Module] | dynamic
        },
    ValidatorPortChild =
        { validator_port_sup_child,                    % child id
          {validator_port_sup, start_link, [ExtProg]}, % startfunc: {Module, Funktion, Args}
          permanent,                                   % restart  : permanent | temporary | transient
          infinity,                                    % shutdown timeout in ms
          supervisor,                                  % type     : worker | supervisor
          [validator_port_sup]                         % modules  : [Module] | dynamic
        },
    {ok, { {RestartStrategy, MaxRestart, MaxTime},
           [QueueChild, ValidatorPortChild]
         }
    }.

%% ===================================================================
