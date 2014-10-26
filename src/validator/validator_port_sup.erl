-module(validator_port_sup).

-behaviour(supervisor).

%% application programming interface
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).


%% ===================================================================
%% application programming interface
%% ===================================================================

start_link(ExtProg) ->
    supervisor:start_link(?MODULE, ExtProg).


%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init(ExtProg) ->
    RestartStrategy = one_for_one,   % one_for_one | one_for_all | rest_for_one
    MaxRestart      = 100,           % max number of restarts in the last ...
    MaxTime         = 60,            % ... seconds
    {ok, { {RestartStrategy, MaxRestart, MaxTime},
	   [ { validator_port_child,                    % child id
               {validator_port, start_link, [ExtProg]}, % startfunc: {Module, Funktion, Args}
               permanent,                               % restart  : permanent | temporary | transient
               10,                                      % shutdown timeout in ms
               worker,                                  % type     : worker | supervisor
               [validator_port]                         % modules  : [Module] | dynamic
	   } ]
	 }
    }.

%% ===================================================================
