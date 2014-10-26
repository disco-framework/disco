%% -------------------------------------------------------------------
%% @doc Generates the concrete input strings for the workers.
%% The defined process opens a port for communicating with the barkeeper
%% executable (location configurable in config file).<br/>
%% The barkeeper generates the worker input from a problem specification
%% and a problem state.
%% @end
%% -------------------------------------------------------------------

-module(barkeeper).

%% application programming interface
-export([
         start_link/1,
         ask/2
        ]).

%% spawned process' function
-export([
         init/1
        ]).

-define(REG_NAME, barkeeper).


%% ===================================================================
%% public application programming interface
%% ===================================================================

%% @doc Start the barkeeper process and open the port, starting the
%% external executable
start_link(ExtProg) ->
    Pid = spawn_link(fun() -> init(ExtProg) end),
    {ok, Pid}.

%% @doc Ask the barkeeper to generate a worker input
-spec ask(string(), string()) -> {error, _} | {ok, WorkerInput :: [string()]}.
ask(ProblemSpec, State) ->
    Ref = make_ref(),
    ?REG_NAME ! {ask, self(), Ref, ProblemSpec, State},
    receive
        {workerinput, Ref, Input} ->
            {ok, Input};
        {error, Ref, Reason} ->
            {error, Reason}
    end.


%% ===================================================================
%% spawned process' function
%% ===================================================================

init(ExtProg) ->
    register(?REG_NAME, self()),
    {ok, PortTimeout} = application:get_env(port_call_timeout),
    ok = lager:debug("barkeeper process opening port with ~p", [ExtProg]),
    Port = port_utils:easy_open_killer_port(ExtProg),
    main_loop(Port, PortTimeout).


%% ===================================================================
%% private functions
%% ===================================================================

main_loop(Port, PortTimeout) ->
    receive
        {ask, Asker, Ref, Problem, State} ->
            MsgToPort = json:to_json_msg([{<<"problem">>, list_to_binary(Problem)},
                                          {<<"state">>,   list_to_binary(State)}]),
            port_utils:port_command(Port, MsgToPort),
            receive_answer(Port, json:default_decoder(), Asker, Ref, PortTimeout);
        Else ->
            ok = lager:error("received unexpected message: ~p", [Else])
    end,
    main_loop(Port, PortTimeout).


-spec receive_answer(port(), json:decoder(), pid(), reference(), integer()) -> ok | error.

receive_answer(Port, Decoder, Asker, Ref, Timeout) ->
    receive
        {Port, {data, {_, ReceivedString}}} ->
            case json:process(ReceivedString, Decoder) of
                {incomplete, NewDecoder} ->
                    receive_answer(Port, NewDecoder, Asker, Ref, Timeout);
                {bad_json, _} ->
                    Asker ! {error, Ref, illegal_json},
                    error;
                {ok, DecodedObj} ->
                    _ = json:handle(DecodedObj,
                                    fun(Json) ->
                                            json:process_attrs(
                                              fun(WorkerInput) ->
                                                      Asker ! {workerinput, Ref, WorkerInput}
                                              end,
                                              [{<<"worker input">>,
                                                fun(WorkerInputBin) ->
                                                        [ binary_to_list(Bin) || Bin <- WorkerInputBin ]
                                                end}],
                                              Json)
                                    end),
                    ok
            end

    after Timeout ->
            ok = lager:error("barkeeper port process timed out"),
            Asker ! {error, Ref, timeout},
            error
    end.


%% ===================================================================
