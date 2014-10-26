%% -------------------------------------------------------------------
%% @doc Changes the problem state.
%% Problems can have a state, but this is purely optional. This module
%% defines a process that manages an external executable that can change
%% the state of the current problem.<br/>
%% The changer uses a proposition (from a worker) to generate a new
%% problem state from the current problem state.
%% @end
%% -------------------------------------------------------------------

-module(changer).

%% application programming interface
-export([
         start_link/1,
         change_state/2
        ]).

%% spawned process' function
-export([
         init/1
        ]).

-define(REG_NAME, changer).


%% ===================================================================
%% public application programming interface
%% ===================================================================

%% @doc Start the changer process and open the port, starting the
%% external executable
start_link(ExtProg) ->
    Pid = spawn_link(fun() -> init(ExtProg) end),
    {ok, Pid}.

%% @doc Ask the external executable to generate a new problem state
change_state(State, Proposition) ->
    Ref = make_ref(),
    ?REG_NAME ! {change_state, self(), Ref, State, Proposition},
    receive
        {new_state, Ref, NewState} ->
            {ok, NewState};
        {error, Ref, Reason} ->
            {error, Reason}
    end.


%% ===================================================================
%% spawned process' function
%% ===================================================================

init(ExtProg) ->
    register(?REG_NAME, self()),
    {ok, PortTimeout} = application:get_env(port_call_timeout),
    ok = lager:debug("changer process opening port with ~p", [ExtProg]),
    Port = port_utils:easy_open_killer_port(ExtProg),
    main_loop(Port, PortTimeout).


%% ===================================================================
%% private functions
%% ===================================================================

main_loop(Port, PortTimeout) ->
    receive
        {change_state, Asker, Ref, State, Proposition} ->
            MsgToPort = json:to_json_msg([{<<"state">>,       list_to_binary(State)},
                                          {<<"proposition">>, list_to_binary(Proposition)}]),

            port_utils:port_command(Port, MsgToPort), % send message to external program
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
                                              fun(NewState) ->
                                                      Asker ! {new_state, Ref, NewState}
                                              end,
                                              [{<<"state">>, fun erlang:binary_to_list/1}],
                                              Json)
                                    end),
                    ok
            end

    after Timeout ->
            ok = lager:error("changer port process timed out"),
            Asker ! {error, Ref, timeout},
            error
    end.


%% ===================================================================
