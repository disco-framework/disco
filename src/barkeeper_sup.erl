-module(barkeeper_sup).

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
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one | ...
    MaxRestart      = 5,
    MaxTime         = 10,
    {ok, { {RestartStrategy, MaxRestart, MaxTime},
	   [ { barkeeper_child,                    % child id
               {barkeeper, start_link, [ExtProg]}, % startfunc: {Module, Funktion, Args}
               permanent,                % restart  : permanent | temporary | transient
               10,                       % shutdown timeout in ms
               worker,                   % type     : worker | supervisor
               [barkeeper]               % modules  : [Module] | dynamic
	   } ]
	 }
    }.

%% ===================================================================
