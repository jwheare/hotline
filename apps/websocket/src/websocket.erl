-module(websocket).
-export([start/0, stop/0]).

start() ->
    application:start(ranch),
    application:start(cowlib),
    application:start(crypto),
    application:start(cowboy),
    application:start(websocket).

stop() ->
    application:stop(websocket).
