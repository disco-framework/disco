-module(pw).

-author('Philip Müller').

-export([start/0]).

start() ->
    _ = application:start(pw).
