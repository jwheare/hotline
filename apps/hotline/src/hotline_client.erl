-module(hotline_client).

-export([start_link/0, loop/1]).

start_link() ->
    line_socket_server:start_link(hotline_client, {"localhost", 55500}, {?MODULE, loop}).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            hotline_c2s:send_chat(Line),
            loop(Socket);
        {error, closed} ->
            ok
    end.
