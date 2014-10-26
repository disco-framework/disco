%% -------------------------------------------------------------------
%% @doc Handles the external validator component/executable.<br/>
%% This module has a very small public interface, because the process
%% automatically pulls propositions from the queue. Validation is
%% never triggered directly.<br/>
%% The validation result is returned to the queue process.
%% @end
%% -------------------------------------------------------------------

-module(validator_port).

%% application programming interface
-export([
         start_link/1
        ]).

-include("validator_data.hrl").

-define(REG_NAME, validator_port).


%% ===================================================================
%% application programming interface
%% ===================================================================

start_link(ExtProg) ->
    Pid = spawn_link(fun() -> init(ExtProg) end),
    {ok, Pid}.


%% ===================================================================
%% spawned process' function
%% ===================================================================

init(ExtProg) ->
    register(?REG_NAME, self()),
    {ok, PortTimeout} = application:get_env(port_call_timeout),
    ok = lager:debug("Validator process opening port with ~p", [ExtProg]),
    Port = port_utils:easy_open_killer_port(ExtProg),
    main_loop(Port, PortTimeout).


%% ===================================================================
%% private functions
%% ===================================================================

main_loop(Port, PortTimeout) ->
    Prop = validator_queue:get_proposition(),
    {_WorkerID, WorkerInput, WorkerOutput} = Prop,
    MsgToPort = json:to_json_msg([{<<"input">>,  lists:map(
                                                   fun list_to_binary/1,
                                                   WorkerInput)},
                                  {<<"output">>, list_to_binary(WorkerOutput)}]),
    port_command(Port, MsgToPort),
    receive_answer(Port, json:default_decoder(), Prop, PortTimeout),
    main_loop(Port, PortTimeout).


-spec receive_answer(port(), json:decoder(), proposition(), non_neg_integer())
                    -> ok | timeout.
receive_answer(Port, Decoder, Prop, Timeout) ->
    receive
        {Port, {data, {_, ReceivedString}}} ->
            case json:process(ReceivedString, Decoder) of
                {incomplete, NewDecoder} ->
                    receive_answer(Port, NewDecoder, Prop, Timeout);
                {bad_json, _} ->
                    exit(illegal_json);
                {ok, DecodedObj} ->
                    _ = json:handle(DecodedObj,
                                    fun(Json) ->
                                            json:process_attrs(
                                              fun(Score, Caption) ->
                                                      validator_queue:put_score(
                                                        Prop, Score, Caption)
                                              end,
                                              [<<"score">>,
                                               {<<"caption">>, fun erlang:binary_to_list/1}],
                                              Json)
                                    end),
                    ok
            end

    after Timeout ->
            ok = lager:error("validator port process times out"),
            timeout
    end.


%% ===================================================================
