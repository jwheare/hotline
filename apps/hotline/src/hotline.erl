-module(hotline).
-export([start/0, stop/0]).

start() ->
    application:start(hotline).

stop() ->
    application:stop(hotline).
