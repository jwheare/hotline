%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(websocket_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
    io:format("START WEBSOCKET~n"),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", hotline_web, []},
            {"/[...]", cowboy_static, {priv_dir, hotline, "www"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 55500}],
		[{env, [{dispatch, Dispatch}]}]),
	websocket_sup:start_link().

stop(_State) ->
	ok.
