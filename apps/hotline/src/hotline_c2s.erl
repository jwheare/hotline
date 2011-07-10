-module(hotline_c2s).

-behaviour(gen_server).

-export([
    start_link/0,
    stop/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(REMOTE_PORT, 5500).
-define(LOG(String, Params), io:format(String ++ "~n", Params)).
-define(LOG(String), ?LOG(String, [])).

-record(state, {
    socket,    % tcp socket
    fsm,       % connected state
    connection % connection object
}).
-record(connection, {
    hostname,
    title,
    username,
    password,
    name,
    icon,
    transaction_id=0
}).

% Public methods

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

% gen_server callbacks

init([]) ->
    Connection = #connection{
        hostname="hotline.psyjnir.org",
        title="Psyjnir",
        username="",
        password="",
        name="Spawnfest [erlang]",
        icon="25704"
    },
    case connect(Connection) of
        {ok, Socket} ->
            self() ! connected,
            {ok, #state{
                socket=Socket,
                fsm=connecting,
                connection=Connection
            }};
        {error, Reason} ->
            {stop, {connect_error, Reason}}
    end.

terminate(Reason, State) ->
    ?LOG("Socket closed: ~p", [Reason]),
    gen_tcp:close(State#state.socket),
    {stop, Reason, State#state.socket}.

code_change(_PreviousVersion, State, _Extra) ->
    {ok, State}.

% handle_call

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% handle_cast

handle_cast({send_data, Data}, State) ->
    ?LOG("[SND] ~p", [Data]),
    case gen_tcp:send(State#state.socket, Data) of
        ok              -> {noreply, State};
        {error, Reason} -> {stop, {send_data_error, Reason}, State}
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

% handle_info

handle_info(connected, State) ->
    send_handshake(),
    {noreply, State};

handle_info({tcp, _Socket, Packet}, State) ->
    NewState = handle_packet(Packet, State),
    {noreply, NewState};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};

handle_info({tcp_error, _Socket, Reason}, State) ->
    {stop, {tcp_error, Reason}, State};

handle_info(Request, State) ->
    ?LOG("Unhandled request: ~p", [Request]),
    {noreply, State}.

% private methods

connect(Connection) ->
    gen_tcp:connect(Connection#connection.hostname, ?REMOTE_PORT, [
        binary,
        {active, true},
        {send_timeout, 2000}
    ]).

send_data(Data) ->
    gen_server:cast(?MODULE, {send_data, Data}).

send_handshake() ->
    Version = 1,
    SubVersion = 2,
    send_data(<<"TRTP","HOTL",Version:16,SubVersion:16>>).

login(Connection) ->
    send_request(Connection, login, [
        {user_login, Connection#connection.username},
        {user_password, Connection#connection.password}
    ]).

send_request(Connection, Operation, Parameters) ->
    Flags = 0,
    IsReply = 0,
    ErrorCode = 0,
    
    OperationConst = hotline_constants:transaction(Operation),
    TransactionId = Connection#connection.transaction_id + 1,
    
    % Built parameter data
    EncodedParameters = [
        {hotline_constants:field(ParamK), length(ParamV), list_to_binary(ParamV)}
        || {ParamK, ParamV} <- Parameters
    ],
    ParameterData = [<<FieldId:16,FieldSize:16,FieldData>> || {FieldId, FieldSize, FieldData} <- EncodedParameters],
    
    ParameterCount = length(Parameters),
    
    TotalSize = length(ParameterData),
    ChunkSize = length(ParameterData),
    
    Header = <<
        Flags,
        IsReply,
        OperationConst:16,
        TransactionId:32,
        ErrorCode:32,
        TotalSize:32,
        ChunkSize:32
    >>,
    send_data([Header, <<ParameterCount:16>>, ParameterData]),
    Connection#connection{transaction_id=TransactionId}.

% handle_packet

% handshake

handle_packet(<<"TRTP",0,0,0,0>>, State = #state{fsm=connecting}) ->
    Connection = login(State#state.connection),
    State#state{fsm=login, connection=Connection};

handle_packet(<<"TRTP",Error:32>>, _State = #state{fsm=connecting}) ->
    exit({handshake_error, Error});

% default

handle_packet(Packet, State) ->
    io:format("Unhandled packet:~n~p~n~p~n", [State, Packet]),
    State.
