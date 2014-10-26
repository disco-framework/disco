-module(gui_port_sup).

-behaviour(supervisor).

%% application programming interface
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).


%% ===================================================================
%% application programming interface
%% ===================================================================

start_link(ExtProgList) ->
    GuiList = lists:zip(
                lists:seq(0, length(ExtProgList)-1),
                ExtProgList
               ),
    supervisor:start_link(?MODULE, GuiList).


%% ===================================================================
%% supervisor callbacks
%% ===================================================================

-spec init([{GuiIdx :: non_neg_integer(), ExtProg :: file:filename()}]) -> {ok, {{supervisor:strategy(), MaxR :: non_neg_integer(), MaxT :: pos_integer()}, [supervisor:child_spec()]}}.
init(GuiList) ->
    RestartStrategy = one_for_one,   % one_for_one | one_for_all | rest_for_one
    MaxRestart      = 10,            % max number of restarts in the last ...
    MaxTime         = 60,            % ... seconds
    {ok, { {RestartStrategy, MaxRestart, MaxTime},
           lists:map(fun({GuiIdx, ExtProg}) ->
                             { list_to_atom("gui_port_child_" ++ integer_to_list(GuiIdx)), % child id
                               {gui_port, start_link, [GuiIdx, ExtProg]}, % startfunc: {Module, Funktion, Args}
                               permanent,                         % restart  : permanent | temporary | transient
                               500,                               % shutdown timeout in ms
                               worker,                            % type     : worker | supervisor
                               [gui_port]                         % modules  : [Module] | dynamic
                             }
                     end,
                     GuiList)
	 }
    }.

%% ===================================================================
