%% -------------------------------------------------------------------
%% @doc Handles a single external GUI.
%% @end
%% -------------------------------------------------------------------
-module(gui_port).

-behaviour(gen_server).

%% application programming interface
-export([
         start_link/2,
         stop/1,
         send/2
        ]).

%% called by gen_server module
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

%% server state
-record(state, {gui_idx      = undefined              :: non_neg_integer() | undefined,
                port         = undefined              :: port() | undefined,
                json_decoder = json:default_decoder() :: json:decoder()
               }).

-define(REG_NAME(GuiIdx), utils:get_gui_reg_name(GuiIdx)).


%% ===================================================================
%% application programming interface
%% ===================================================================

start_link(GuiIdx, ExtProg) ->
    %% start gen_server and register
    gen_server:start_link({local, ?REG_NAME(GuiIdx)}, ?MODULE, {GuiIdx, ExtProg}, []).

stop(GuiIdx) ->
    gen_server:cast(?REG_NAME(GuiIdx), stop).

-spec send(non_neg_integer(), binary()) -> ok.
send(GuiIdx, Msg) ->
    RegName = ?REG_NAME(GuiIdx),
    try gen_server:call(RegName, {to_port, Msg})
    catch
        %% Catch exception in the case that the desired gui process not (yet) exists.
        exit:{noproc, Reason={gen_server, call, [GuiName, {to_port, _Msg}]}} ->
            ok = lager:debug("Can not send message to gui_port \"~s\", "
                             ++ "process does not (yet) exist.~n~p",
                             [GuiName, Reason]),
            ok
    end.


%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init({GuiIdx, ExtProg}) ->
    process_flag(trap_exit, true),
    ok = lager:debug("Gui process opening port with ~p", [ExtProg]),
    Port = port_utils:easy_open_killer_port(ExtProg),

    %% request all data to initialize GUI
    gui:get_all_data(),
    {ok, #state{gui_idx=GuiIdx, port=Port}}.

%% ----------

%% Give message to port
handle_call({to_port, Msg}, _From, State = #state{port=Port}) ->
    port_command(Port, Msg),
    {reply, ok, State}.

%% ----------

handle_cast(stop, _State) ->
    ok = lager:info("GUI Stopped! restarting"),
    {stop, shutdown};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% ----------

handle_info({'EXIT', Port, Reason}, State = #state{gui_idx=GuiIdx, port=Port}) ->
    ok = lager:info("Gui port ~p terminated with reason ~p", [Port, Reason]),
    case Reason of
        normal -> stop(GuiIdx),
                  {noreply, State};
        _      -> {stop, {port_terminated, Reason}, State}
    end;
%% Handle messages from port
handle_info({Port, {data, {_, ReceivedString}}}, State = #state{port=Port, json_decoder=Decoder}) ->
    NewDecoder = case json:process(ReceivedString, Decoder) of
                     {ok, DecodedObj} ->
                         _ = json:handle(DecodedObj, fun gui:handle_json/1),
                         json:default_decoder();
                     {_, UsedDecoder} ->
                         UsedDecoder
                 end,
    {noreply, State#state{json_decoder=NewDecoder}}.

%% ----------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ----------

terminate({port_terminated, _Reason}, _State) ->
    ok;
terminate(_Reason, _State = #state{port=Port}) ->
    case erlang:port_info(Port) of
        undefined ->
            already_closed;
        _ ->
            port_close(Port)
    end.


%% ===================================================================
