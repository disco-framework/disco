-module(worker_port_owner).

%% application programming interface
-export([
         start_link/3
        ]).


%% ===================================================================
%% application programming interface
%% ===================================================================

start_link(WorkerID, WorkingDir, ExtProg) ->
    {ok, spawn_link(fun() -> init(WorkerID, WorkingDir, ExtProg) end)}.


%% ===================================================================
%% private functions
%% ===================================================================

init(WorkerID, WorkingDir, ExtProg) ->
    process_flag(trap_exit, true),
    %% restart penalty to limit max. crash frequency
    timer:sleep(100),
    lagerd:debug("opening port: ~s$ ~s", [WorkingDir, ExtProg]),
    %% port documentation: http://www.erlang.org/doc/man/erlang.html#open_port-2
    Port = port_utils:easy_open_killer_port(ExtProg, WorkingDir),
    ok = worker_server:set_port_ref(WorkerID, Port),
    main_loop(Port, WorkerID, [], 0).

main_loop(Port, WorkerID, PartList, PropositionCounter) ->
    receive
        {Port, {data, {noeol, Part}}} ->
            main_loop(Port, WorkerID, [Part | PartList], PropositionCounter);

        {Port, {data, {eol, Part}}} ->
            AppendFront = fun(S1, S2) -> S1 ++ S2 end,
            %% reverse accumulator list and concatenate to string:
            Message = lists:foldl(AppendFront, "", [Part | PartList]),
            lagerd:debug("Worker port ~p received line: ~p", [WorkerID, Message]),
            worker_app:submit_proposition(WorkerID, Message),
            main_loop(Port, WorkerID, [], PropositionCounter + 1);

        {'EXIT', Port, Reason} ->
            worker_app:worker_stopped(WorkerID),
            lagerd:debug("Worker port ~p terminated with reason ~p. (~p propositions submitted)",
                         [WorkerID, Reason, PropositionCounter]);

        {'EXIT', _FromPid, _Reason} ->
            main_loop(Port, WorkerID, PartList, PropositionCounter);

        Other ->
            lagerd:critical("Worker port ~p received unknown message: ~p", [WorkerID, Other]),
            main_loop(Port, WorkerID, PartList, PropositionCounter)
    end.


%% ===================================================================
