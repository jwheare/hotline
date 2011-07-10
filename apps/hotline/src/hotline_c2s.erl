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
-record(transaction, {
    flags,
    is_reply,
    operation,
    transaction_id,
    error_code,
    parameters
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
    NewState = handle_tcp(Packet, State),
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
    
    OperationCode = hotline_constants:transaction_to_code(Operation),
    TransactionId = Connection#connection.transaction_id + 1,
    
    ?LOG("[SND ~p] ~p: ~p", [TransactionId, Operation, Parameters]),
    
    % Built parameter data
    EncodedParameters = [
        {hotline_constants:field_to_code(ParamK), length(ParamV), list_to_binary(ParamV)}
        || {ParamK, ParamV} <- Parameters
    ],
    
    ParameterData = [[<<FieldType:16,FieldSize:16>>,FieldData] || {FieldType, FieldSize, FieldData} <- EncodedParameters],
    
    ParameterCount = length(Parameters),
    
    TotalSize = length(ParameterData),
    ChunkSize = length(ParameterData),
    
    Header = <<
        Flags:8,
        IsReply:8,
        OperationCode:16,
        TransactionId:32,
        ErrorCode:32,
        TotalSize:32,
        ChunkSize:32
    >>,
    send_data([Header, <<ParameterCount:16>>, ParameterData]),
    Connection#connection{transaction_id=TransactionId}.

% parse_transactions

parse_params(Data) -> lists:reverse(parse_params(Data, [])).
parse_params(<<>>, Acc) -> Acc;
parse_params(ParameterData, Acc) ->
    <<FieldType:16,FieldSize:16,Rest/binary>> = ParameterData,
    <<FieldData:FieldSize/binary,RestParams/binary>> = Rest,
    parse_params(RestParams, [{hotline_constants:field_to_atom(FieldType), FieldData}|Acc]).

parse_transactions(Packet) -> lists:reverse(parse_transactions(Packet, [])).
parse_transactions(<<>>, Acc) -> Acc;
parse_transactions(<<
        Flags:8,
        IsReply:8,
        OperationCode:16,
        TransactionId:32,
        ErrorCode:32,
        _TotalSize:32,
        DataSize:32,
        Rest/binary
    >>, Acc) ->
    <<DataPart:DataSize/binary,RestPacket/binary>> = Rest,
    <<_ParameterCount:16,ParameterData/binary>> = DataPart,
    parse_transactions(RestPacket, [#transaction{
        flags=Flags,
        is_reply=IsReply,
        operation=hotline_constants:transaction_to_atom(OperationCode),
        transaction_id=TransactionId,
        error_code=ErrorCode,
        parameters=parse_params(ParameterData)
    } | Acc]).

% handle_tcp

handle_tcp(<<"TRTP",0,0,0,0>>, State = #state{fsm=connecting}) ->
    Connection = login(State#state.connection),
    State#state{fsm=login, connection=Connection};

handle_tcp(<<"TRTP",Error:32>>, _State = #state{fsm=connecting}) ->
    exit({handshake_error, Error});

handle_tcp(Packet, State) ->
    lists:foldl(fun (Transaction, CurrentState) ->
        handle_transaction(Transaction, CurrentState)
    end, State, parse_transactions(Packet)).

% handle_transaction

handle_transaction(Transaction = #transaction{operation=chat_msg}, State) ->
    Message = binary_to_list(proplists:get_value(data, Transaction#transaction.parameters)),
    ?LOG("~s", [string:strip(Message, left, $\r)]),
    State;

handle_transaction(Transaction, State) ->
    ?LOG("[RCV ~p] ~p: ~p", [
        Transaction#transaction.transaction_id,
        Transaction#transaction.operation,
        Transaction
    ]),
    State.