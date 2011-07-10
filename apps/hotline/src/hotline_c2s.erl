-module(hotline_c2s).

-behaviour(gen_server).

-export([
    start_link/0,
    stop/0,
    send_chat/1,
    parse_transactions/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("hotline.hrl").

-define(REMOTE_PORT, 5500).
-define(LOG(String, Params), io:format(String ++ "~n", Params)).
-define(LOG(String), ?LOG(String, [])).

-record(state, {
    socket,                    % tcp socket
    status,                    % connected status
    connection,                % connection object
    transaction_id = 0,        % unique incrementing id
    transaction_handlers = [], % transaction response handlers
    packet_buffer = <<>>       % buffer of unparsed packets
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
        icon=25704
    },
    case connect(Connection) of
        {ok, Socket} ->
            self() ! connected,
            {ok, #state{
                socket=Socket,
                status=connecting,
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

handle_cast({send_chat, Line}, State) ->
    {ok, NewState} = send_request(State, chat_send, [
        {data, Line},
        {chat_options, 0}
    ]),
    {noreply, NewState};

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

register_transaction_handler(State, Type, Handler) ->
    TransactionHandlers = [{State#state.transaction_id, {Type, Handler}} | State#state.transaction_handlers],
    State#state{transaction_handlers=TransactionHandlers}.

connect(Connection) ->
    gen_tcp:connect(Connection#connection.hostname, ?REMOTE_PORT, [
        binary,
        {active, true},
        {send_timeout, 2000}
    ]).

send_data(Data) ->
    gen_server:cast(?MODULE, {send_data, Data}).

send_chat(Line) ->
    gen_server:cast(?MODULE, {send_chat, Line}).

send_handshake() ->
    Version = 1,
    SubVersion = 2,
    send_data(<<"TRTP","HOTL",Version:16,SubVersion:16>>).

login(State) ->
    send_request(State, login, [
        {user_login, State#state.connection#connection.username},
        {user_password, State#state.connection#connection.password},
        {user_name, State#state.connection#connection.name},
        {user_icon_id, State#state.connection#connection.icon}
    ]).

send_request(State, Operation, Parameters) ->
    Flags = 0,
    IsReply = 0,
    ErrorCode = 0,
    
    OperationCode = hotline_constants:transaction_to_code(Operation),
    TransactionId = State#state.transaction_id + 1,
    
    ?LOG("[SND ~p] ~p: ~p", [TransactionId, Operation, Parameters]),
    
    % Built parameter data
    ParameterData = lists:map(fun ({ParamK, ParamV}) ->
        Type = hotline_constants:field_to_code(ParamK),
        case ParamV of
            ParamV when is_integer(ParamV) ->
                Size = 4,
                Data = <<ParamV:32>>;
            ParamV when is_list(ParamV) ->
                Data = list_to_binary(ParamV),
                Size = size(Data);
            ParamV when is_binary(ParamV) ->
                Data = ParamV,
                Size = size(Data)
        end,
        <<Type:16,Size:16,Data/binary>>
    end, Parameters),
    
    ParameterCount = length(Parameters),
    
    TotalSize = iolist_size(ParameterData),
    ChunkSize = TotalSize,
    
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
    {ok, State#state{transaction_id=TransactionId}}.

% parse_transactions

parse_params(Data) -> lists:reverse(parse_params(Data, [])).
parse_params(<<>>, Acc) -> Acc;
parse_params(ParameterData, Acc) ->
    <<FieldType:16,FieldSize:16,Rest/binary>> = ParameterData,
    <<FieldData:FieldSize/binary,RestParams/binary>> = Rest,
    parse_params(RestParams, [{hotline_constants:field_to_atom(FieldType), FieldData}|Acc]).

parse_transactions(Packet) ->
    {Transactions, RestPacket} = parse_transactions(Packet, []),
    {lists:reverse(Transactions), RestPacket}.
parse_transactions(<<>>, Transactions) -> {Transactions, <<>>};
parse_transactions(<<
        Flags:8,
        IsReply:8,
        OperationCode:16,
        TransactionId:32,
        ErrorCode:32,
        _TotalSize:32,
        DataSize:32,
        DataPart:DataSize/binary,
        RestPacket/binary
    >>, Transactions) ->
    <<_ParameterCount:16,ParameterData/binary>> = DataPart,
    parse_transactions(RestPacket, [#transaction{
        flags=Flags,
        is_reply=IsReply,
        operation=hotline_constants:transaction_to_atom(OperationCode),
        id=TransactionId,
        error_code=ErrorCode,
        parameters=parse_params(ParameterData)
    } | Transactions]);
parse_transactions(Packet, Transactions) -> {Transactions, Packet}.

% handle_tcp

handle_tcp(<<"TRTP",0,0,0,0>>, State = #state{status=connecting}) ->
    {ok, NewState} = login(State),
    NewState#state{status=login};

handle_tcp(<<"TRTP",Error:32>>, _State = #state{status=connecting}) ->
    exit({handshake_error, Error});

handle_tcp(Packet, State) ->
    PacketBuffer = State#state.packet_buffer,
    {Transactions, RestPacket} = parse_transactions(<<PacketBuffer/binary,Packet/binary>>),
    NewState = State#state{packet_buffer=RestPacket},
    lists:foldl(fun (Transaction, CurrentState) ->
        handle_transaction(CurrentState, Transaction)
    end, NewState, Transactions).

% handle_transaction

handle_transaction(State, Transaction = #transaction{operation=chat_msg}) ->
    Message = binary_to_list(proplists:get_value(data, Transaction#transaction.parameters)),
    ?LOG("~s", [string:strip(Message, left, $\r)]),
    State;

handle_transaction(State, Transaction) ->
    % Check for a transaction handler
    TxnId = Transaction#transaction.id,
    case proplists:get_value(TxnId, State#state.transaction_handlers) of
        {_Type, Fun} when is_function(Fun) ->
            {ok, NewState} = Fun(State),
            TransactionHandlers = proplists:delete(TxnId, State#state.transaction_handlers),
            NewState#state{transaction_handlers=TransactionHandlers};
        undefined ->
            % No handler
            ?LOG("[RCV ~p] ~p: ~p", [
                TxnId,
                Transaction#transaction.operation,
                Transaction
            ]),
            State
    end.