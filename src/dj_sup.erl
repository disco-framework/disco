-module(dj_sup).

-behaviour(supervisor).

%% application programming interface
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% ===================================================================
%% application programming interface
%% ===================================================================

start_link() ->
    supervisor:start_link(?MODULE, []).


%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init(_) ->
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one | ...
    MaxRestart      = 5,
    MaxTime         = 10,
    {ok, { {RestartStrategy, MaxRestart, MaxTime},
	   [ { dj_child,             % child id
               {dj, start_link, []}, % startfunc: {Module, Funktion, Args}
               permanent,            % restart  : permanent | temporary | transient
               10,                   % shutdown timeout in ms
               worker,               % type     : worker | supervisor
               [dj]                  % modules  : [Module] | dynamic
	   } ]
	 }
    }.

%% ===================================================================
