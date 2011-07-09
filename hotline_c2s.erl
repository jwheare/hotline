-module(hotline_c2s).

-export([
    connect/1
]).

-define(REMOTE_PORT, 5500).
% -define(LOCAL_PORT, 55025).

connect(Host) ->
    case gen_tcp:connect(Host, ?REMOTE_PORT, [
        binary,
        {active, true},
        {send_timeout, 2000}
        % {port, ?LOCAL_PORT}
    ]) of
        {ok, Sock} ->
            loop(Sock);
        Ret -> Ret
    end.

loop(Sock) ->
    receive
        {Client, send_data, Binary} ->
            case gen_tcp:send(Sock, [Binary]) of
                {error, Reason} ->
                    Client ! {self(), {send_data_error, Reason}},
                    gen_tcp:close(Sock);
                ok ->
                    io:format("[SND] ~p", [Binary]),
                    loop(Sock)
            end;
            
        {tcp, Sock, Data} ->
            io:format("[RCV] ~p", [Data]),
            loop(Sock);
        {tcp_closed, Sock} ->
            tcp_closed;
        {tcp_error, Sock, Reason} ->
            {tcp_error, Reason}
    end.
