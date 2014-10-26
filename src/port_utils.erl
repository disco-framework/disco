%% -------------------------------------------------------------------
%% @doc Utilities for working with Erlang ports.
%% @end
%% -------------------------------------------------------------------

-module(port_utils).

-export([
         easy_open_killer_port/1,
         easy_open_killer_port/2,
         open_killer_port/2,
         spawn_port/2,
         port_command/2,
         try_open/3
        ]).

%% @doc Opens a port with special arrangements to safely kill the
%% command later.<br/>
%% Convenience wrapper for frequent use cases of {@link utils:open_killer_port/2}.<br/>
%% Needs the environment value port_buffer_size :: pos_integer().
%% @end
-spec easy_open_killer_port(string()) -> port().
easy_open_killer_port(ExtProg) ->
    open_killer_port(ExtProg, [stream]).
%% @doc Opens a port with special arrangements to safely kill the
%% command later.<br/>
%% Convenience wrapper for frequent use cases of {@link utils:open_killer_port/2}.<br/>
%% Needs the environment value port_buffer_size :: pos_integer().
%% @end
-spec easy_open_killer_port(string(), string()) -> port().
easy_open_killer_port(ExtProg, WorkingDir) ->
    open_killer_port(ExtProg, [stream, {cd, WorkingDir}]).

%% @doc Opens a port with special arrangements to safely kill the
%% command later.<br/>
%% Needs the environment value port_buffer_size :: pos_integer().
%% @end
-spec open_killer_port(string(), list()) -> port().
open_killer_port(ExtProg, PortOptions) ->
    %% To escape single quotes in single-quoted strings, you need to break out
    %% of the quoted string: let's party --> 'let'\''s party'
    ExtProgEscaped = lists:foldr(fun(C, Acc) -> case C of
                                                    $' -> [$',$\\,$',$'|Acc];
                                                    _  -> [C|Acc]
                                                end
                                 end, "", ExtProg),
    Cmd = lists:concat(["bash -c '(cat && kill -9 0) | (", ExtProgEscaped, "; kill 0)'"]),
    {ok, MaxLineLength} = application:get_env(port_buffer_size),
    ?MODULE:spawn_port(Cmd, [{line, MaxLineLength} | PortOptions]).

%% @doc we need this proxy function for mocking
%% because meck can't mock the "erlang" module
-spec spawn_port(string(), list()) -> port().
spawn_port(Cmd, PortOptions) ->
    erlang:open_port({spawn, Cmd}, PortOptions).

%% @doc we need this proxy function for mocking
%% because meck can't mock the "erlang" module
-spec port_command(port(), iodata()) -> true.
port_command(Port, Data) ->
    erlang:port_command(Port, Data).

%% @doc Tries opening an external executable via a port.
%% It opens the port and waits for errors until timeout.
%% Useful for generating error messages for misconfigured or faulty executables.
-spec try_open(string(), nonempty_string(), timeout()) -> ok | {error, string()}.
try_open(WorkingDir, ExtProg, Timeout) ->
    Port = open_killer_port(ExtProg, [stream, stderr_to_stdout, exit_status, {cd, WorkingDir}]),
    case get_data(Port, Timeout) of
	{ok, _Output} ->
	    ok;
	{timeout, _Output} ->
	    ok;
	{exit, ExitStatus, Output} ->
	    {error, io_lib:format("Failed to start '~s' (in directory: '~s'). exit status=~p~nProgram output: ~s",
                                  [ExtProg, WorkingDir, ExitStatus, Output])}
    end.


%% ===================================================================
%% private functions
%% ===================================================================

%% @doc Reads data from port and combines into full messages (Text + EOL).
-spec get_data(port(), timeout())
              -> {ok, string()} | {exit, integer(), string()} | {timeout, string()}.
get_data(Port, Timeout) ->
    get_data(Port, Timeout, []).

%% @doc Recursion helper
get_data(Port, Timeout, Sofar) ->
    receive
        {Port, {exit_status, Status}} ->
            {exit, Status, lists:flatten(Sofar)};
	{Port, {data, Bytes}} ->
	    case eot(Bytes) of
		{done, Last} ->
		    {ok, lists:flatten([Sofar|Last])};
		more  ->
		    get_data(Port, Timeout, [Sofar|Bytes])
	    end;
	{'EXIT', Port, _} ->
	    {ok, lists:flatten(Sofar)}
    after Timeout ->
            {timeout, Sofar}
    end.

%% @doc Tests whether the given string contains the end of transmission character
eot(String) ->
    eot(String, []).

%% @doc Recursion helper
eot([], _Processed) ->
    more;
eot([4| _Rest], Processed) ->
    {done, lists:reverse(Processed)};
eot([First | Rest], Processed) ->
    eot(Rest, [First | Processed]).

%% ===================================================================
