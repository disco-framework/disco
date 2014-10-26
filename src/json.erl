%% -------------------------------------------------------------------
%% @doc Utilities for JSON handling.
%% @end
%% -------------------------------------------------------------------
-module(json).

-export([
         default_decoder/0,
         process/2,
         handle/2,
         to_json_msg/1,
         process_attrs/3
        ]).

-export_type([
              decoder/0
             ]).

-type decoder()       :: {fun((binary()) -> any()), ProcessedStrings :: list()}.
-type empty_decoder() :: {fun((binary()) -> any()), ProcessedStrings :: []}.
-type json_value() :: list({binary(), json_value()})
                    | list(json_value())
                    | true
                    | false
                    | null
                    | integer()
                    | float()
                    | binary().


%% @doc Construct an empty decoder with sane defaults
-spec default_decoder() -> empty_decoder().
default_decoder() ->
    {jsx:decoder(jsx_to_term, [], []), []}.

%% @doc Try to process an incoming string into valid JSON.
%% Can return a stateful decoder if the string did not contain a complete valid JSON term.
%% The decoder can be used with further strings to construct a complete JSON term.
-spec process(string(), decoder()) ->
                     {ok, json_value()} |
                     {incomplete, decoder()} |
                     {bad_json, decoder()}.
process(ReceivedString, Decoder) ->
    {JsxDecoder, PrevStrings} = Decoder,
    try JsxDecoder(list_to_binary(ReceivedString)) of

        {incomplete, NewJsxDecoder} ->
            %% new decoder contains received string
            {incomplete, {NewJsxDecoder, [ReceivedString | PrevStrings]}};

        DecodedObj ->
            %% continue with a new decoder
            {ok, DecodedObj}
    catch
        error:badarg ->
            ok = lager:error("Received bad JSON: ~p",
                             [format_received_strings([ReceivedString | PrevStrings])]),
            %% continue with a new decoder
            {bad_json, default_decoder()}
    end.

%% @doc Try to handle the JSON term with the given handler function.
-spec handle(json_value(), fun((json_value()) -> ok | unknown_json)) -> ok | unknown_json.
handle(DecodedObj, Handler) ->
    case Handler(DecodedObj) of
        ok ->
            ok;
        unknown_json ->
            ok = lager:warning("Received unknown JSON: ~p", [DecodedObj]),
            unknown_json
    end.

%% @doc Construct a port message as defined by our protocol, a string ending with a newline,
%% from a JSON term.
-spec to_json_msg(json_value()) -> binary().
to_json_msg(ErlTerm) ->
    JSON = jsx:to_json(ErlTerm),
    <<JSON/binary,$\n>>.


%% edoc demands type comments to be positioned below the type definition
-type attr_spec() :: Key :: binary() | {Key :: binary(), Handler :: fun()}.
%% Attribute key with optional conversion handler

%% @doc Check wether all the specified attributes are contained in the given JSON object and
%% if successful optionally call the given conversion handlers with the attribute values.
%% Then call the given fun() with the processed values.
-spec process_attrs(fun(),
                    [attr_spec()],
                    json_value()) -> ok | unknown_json.
process_attrs(Fun, Attrs, JsonTerm) ->
    case get_args(Attrs, JsonTerm) of
        undefined ->
            unknown_json;
        Args ->
            apply(Fun, Args),
            ok
    end.


%% private functions

%% @doc Format the received strings for log output.
-spec format_received_strings(string()) -> string().
format_received_strings(Strings) ->
    %% reverse and concat
    lists:foldl(fun(Str, Acc) -> Str ++ Acc end, "", Strings).

%% @doc Extract the values of the attributes with the specified keys from a JSON object,
%% applying the optional conversion handlers.
-spec get_args([attr_spec()], json_value()) -> undefined | [json_value()].
get_args(AttrSpecs, JsonTerm) ->
    get_args(AttrSpecs, JsonTerm, []).

%% @doc Helper function for {@link get_args/3}.
-spec get_args([attr_spec()], json_value(), [json_value()]) -> undefined | [json_value()].
get_args([], _, Acc) -> lists:reverse(Acc);
get_args([AttrSpec|AttrSpecs], JsonTerm, Acc) ->
    Key = case AttrSpec of
              {Key_, _} -> Key_;
              _         -> AttrSpec
          end,
    case proplists:get_value(Key, JsonTerm) of
        undefined ->
            undefined;
        Arg ->
            FinalArg = case AttrSpec of
                           {_, Handler} ->
                               Handler(Arg);
                           _ ->
                               Arg
                       end,
            get_args(AttrSpecs, JsonTerm, [FinalArg|Acc])
    end.
