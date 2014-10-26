%% -------------------------------------------------------------------
%% @doc Provides a proxy process for logging.<br/>
%% Messages that should be logged are sent from all nodes to this
%% process so they all end up in a single log file on the master node.
%% @end
%% -------------------------------------------------------------------

-module(lagerd).

%% application programming interface
-export([
         start_link/0,
         terminate/0
        ]).

-export([
         debug/1,     debug/2,
         info/1,      info/2,
         notice/1,    notice/2,
         warning/1,   warning/2,
         error/1,     error/2,
         critical/1,  critical/2,
         alert/1,     alert/2,
         emergency/1, emergency/2
        ]).

-define(GLOBAL_REG_NAME, lager_daemon).
-define(FORMAT(NodeName, Format),
        lists:concat(["-REMOTE- ", atom_to_list(NodeName), " -- ", Format])
       ).


%% ===================================================================
%% application programming interface
%% ===================================================================

start_link() ->
    Pid = spawn(fun main_loop/0),
    global:register_name(?GLOBAL_REG_NAME, Pid),
    {ok, Pid}.

terminate() ->
    send(terminate),
    global:unregister_name(?GLOBAL_REG_NAME).


%% ===================================================================
%% Defining proxy functions for all the lager logging functions via
%% macros

-define(LOG1(Severity), Severity(Format) ->
               send({Severity, node(), Format})).
-define(LOG2(Severity), Severity(Format,Args) ->
               send({Severity, node(), Format, Args})).

?LOG1(debug).      ?LOG2(debug).
?LOG1(info).       ?LOG2(info).
?LOG1(notice).     ?LOG2(notice).
?LOG1(warning).    ?LOG2(warning).
?LOG1(error).      ?LOG2(error).
?LOG1(critical).   ?LOG2(critical).
?LOG1(alert).      ?LOG2(alert).
?LOG1(emergency).  ?LOG2(emergency).


%% ===================================================================
%% private functions
%% ===================================================================

%% @doc Send a logging message from a remote node to the lagerd process
send(Msg) ->
    case global:whereis_name(?GLOBAL_REG_NAME) of
        undefined -> io:format("ERROR [~s(~p)]: ~s not found!  (node: ~p)~nTried to log message:~n~p~n",
                               [?FILE, ?LINE, ?GLOBAL_REG_NAME, node(), Msg]),
                     ok;
        Pid       -> Pid ! Msg,
                     ok
    end.

main_loop() ->
    receive
        {Severity, Node, Message}
          when is_atom(Node),
               is_atom(Severity),
               is_list(Message)
               ->
            ok = lager:log(Severity, self(), ?FORMAT(Node, Message)),
            main_loop();
        {Severity, Node, Format, Args}
          when is_atom(Node),
               is_atom(Severity),
               is_list(Format),
               is_list(Args)
               ->
            ok = lager:log(Severity, self(), ?FORMAT(Node, Format), Args),
            main_loop();
        terminate -> ok;
        Msg       -> ok = lager:warning("lagerd: unknown message: ~p", [Msg]),
                     main_loop()
    end.


%% ===================================================================
