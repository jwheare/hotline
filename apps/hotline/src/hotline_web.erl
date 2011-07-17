-module(hotline_web).

-export([start/0, start/1, stop/0, loop/2, wsloop_active/1]).

start() -> start([{port, 55500}, {docroot, code:priv_dir(hotline) ++ "/www"}]).

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
    %% How we validate origin for cross-domain checks:
    OriginValidator = fun(Origin) ->
                           io:format("Origin '~s' -> OK~n", [Origin]),
                           true
                      end,
    %% websocket options
    WsOpts  = [ {origin_validator, OriginValidator},
                {loop,   {?MODULE, wsloop_active}} ],
    %%
    mochiweb_http:start([{name, ?MODULE}, 
                         {loop, Loop},
                         {websocket_opts, WsOpts} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

wsloop_active(WSReq) ->
    hotline_c2s:register_websocket(self()),
    wsloop_active0(WSReq).

wsloop_active0(WSReq) ->
    receive
        %% Received msg from the websocket:
        {websockets_frame, Frame} ->
            {struct, Message} = mochijson2:decode(Frame),
            hotline_web_handler:message(proplists:get_value(<<"type">>, Message), Message),
            wsloop_active0(WSReq);
        {hotline_message, Data} ->
            WSReq:send(mochijson2:encode({struct, Data})),
            wsloop_active0(WSReq);
        %% Not strictly necessary, since we get {'EXIT',_,_} regardless:
        {error, Reason} ->
            io:format("client api got error ~p~n", [Reason]),
            ok;
        %% Important to catch these and terminate, or we'll end up with an
        %% orphan process that will crash next time it tries to :send
        {'EXIT', _, Reason} ->
            io:format("WS LOOP exiting, reason ~p~n", [Reason]),
            ok
    after 29000 ->
        %% Some aggressive proxies may disconnect if no traffic for 30 secs
        WSReq:send(mochijson2:encode({struct, [
            {type, <<"idle">>}
        ]})),
        %% NB: perhaps do a fully-qualified call here if you want to do
        %% hot code upgrades on this process in production:
        wsloop_active0(WSReq)
    end.

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
