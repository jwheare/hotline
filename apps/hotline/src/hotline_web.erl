-module(hotline_web).

-behaviour(cowboy_websocket_handler).
-export([init/3, handle/2, terminate/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, Req, Opts) ->
    case cowboy_req:header(<<"upgrade">>, Req) of
        {<<"websocket">>, _Req2} ->
            {upgrade, protocol, cowboy_websocket};
        {_, Req2} ->
            {ok, Req2, Opts}
    end.

%% non-websocket stuff, to serve the index page:
handle(Req, Opts) ->
    {ok, Body} = file:read_file(code:priv_dir(hotline) ++ "/www/index.html"),
    {ok, FinalReq} = cowboy_req:reply(200, [], [Body, "\n"], Req),
    {ok, FinalReq, Opts}.

websocket_init(_TransportName, Req, _Opts) ->
    hotline_c2s:register_websocket(self()),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    {Message} = jiffy:decode(Msg),
    hotline_web_handler:message(proplists:get_value(<<"type">>, Message), Message),
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info({hotline_message, Data}, Req, State) ->
    {reply, {text, jiffy:encode({Data})}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

terminate(_Reason, _Req, _State) ->
    ok.
