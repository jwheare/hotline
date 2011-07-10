% generic line server, based on http://20bits.com/2008/06/16/erlang-a-generalized-tcp-server/
-module(line_socket_server).
-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/1]).
-export([start_link/3]).

-define(TCP_OPTIONS, [
                      list, 
                      {packet, line}, 
                      {active, false}, 
                      {reuseaddr, true}
                     ]).

-record(server_state, {
        port,
        loop,
        ip=any,
        lsocket=null}).

%start_link(Name, Port, Loop) when is_integer(Port) ->
%    start_link(Name, {"0.0.0.0", Port}, Loop);

start_link(Name, {_IpS, Port}, Loop) ->
    %{ok, Ip} = inet:getaddr(IpS, inet),
    Ip = any,
    State = #server_state{port = Port, loop = Loop, ip=Ip},
    io:format("line_server for ~w listening on ~p:~w\n", [Name, Ip,Port]),
    gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #server_state{port=Port,ip=_Ip}) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of
        {ok, LSocket} ->
            NewState = State#server_state{lsocket = LSocket},
            {ok, accept(NewState)};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, _Pid}, State=#server_state{}) ->
    {noreply, accept(State)}.

accept_loop({Server, LSocket, {M, F}}) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            % Let the server spawn a new process and replace this loop
            % with the echo loop, to avoid blocking
            gen_server:cast(Server, {accepted, self()}),
            M:F(Socket);
        _ ->
            stop
    end.
   
% To be more robust we should be using spawn_link and trapping exits
accept(State = #server_state{lsocket=LSocket, loop = Loop}) ->
    proc_lib:spawn(?MODULE, accept_loop, [{self(), LSocket, Loop}]),
    State.

% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
