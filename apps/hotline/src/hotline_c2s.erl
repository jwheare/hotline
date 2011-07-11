-module(hotline_c2s).

-behaviour(gen_server).

-export([
    start_link/1,
    stop/0,
    
    transactions_parse/1,
    
    chat_send/1,
    
    get_state/0,
    get_user_list/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("hotline.hrl").

-define(REMOTE_PORT, 5500).
-define(LOG(String, Params), io:format(String ++ "~n", Params)).
-define(LOG(String), ?LOG(String, [])).

-record(state, {
    socket,                    % tcp socket
    status=disconnected,       % connected status
    connection,                % connection object
    transaction_id = 0,        % unique incrementing id
    response_handlers = [],    % response transaction handlers
    packet_buffer = <<>>,      % buffer of unparsed packets
    user_list = [],            % users connected to the server
    chat_subject,              % topic of the chat
    websocket                  % pids to send socket data to
}).

% Public methods

start_link(WebsocketPid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [WebsocketPid], []).

stop() ->
    gen_server:call(?MODULE, stop).

chat_send(Line) ->
    gen_server:call(?MODULE, {chat_send, Line}).

get_state() ->
    gen_server:call(?MODULE, get_state).
    
get_user_list() ->
    gen_server:call(?MODULE, get_user_list).

% gen_server callbacks

init([WebsocketPid]) ->
    Connection = #connection{
        hostname="livebus.org",
        title="Livebus",
        username="",
        password="",
        name="Spawnfest User",
        icon=150
    },
    monitor(process, WebsocketPid),
    case connect(Connection) of
        {ok, Socket} ->
            self() ! connected,
            {ok, #state{
                socket=Socket,
                status=connecting,
                connection=Connection,
                websocket=WebsocketPid
            }};
        {error, Reason} ->
            {stop, {connect_error, Reason}}
    end.

terminate(Reason, State) ->
    ?LOG("Socket closed: ~p", [Reason]),
    websocket(State, [
        {type, <<"socket_closed">>}
    ]),
    gen_tcp:close(State#state.socket),
    {stop, Reason, State#state.socket}.

code_change(_PreviousVersion, State, _Extra) ->
    {ok, State}.

% websocket

websocket(State, Message) ->
    State#state.websocket ! {hotline_message, Message}.

% handle_call

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({chat_send, Line}, _From, State) ->
    NewState = chat_send(State, Line),
    {reply, ok, NewState};

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(get_user_list, _From, State) ->
    {reply, State#state.user_list, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% handle_cast

handle_cast(_Request, State) ->
    {noreply, State}.

% handle_info

handle_info(connected, State) ->
    websocket(State, [{type, <<"handshake">>}]),
    handshake(State),
    {noreply, State};

handle_info({'DOWN', _MonitorRef, _Type, _Pid, _Info}, State) ->
    {stop, normal, websocket_down, State};

handle_info({tcp, _Socket, Packet}, State) ->
    NewState = tcp(State, Packet),
    {noreply, NewState};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};

handle_info({tcp_error, _Socket, Reason}, State) ->
    {stop, {tcp_error, Reason}, State};

handle_info(Request, State) ->
    ?LOG("Unhandled request: ~p", [Request]),
    {noreply, State}.

% register_response_handler

register_response_handler(State, Type) ->
    ResponseHandlers = [{State#state.transaction_id, Type} | State#state.response_handlers],
    State#state{response_handlers=ResponseHandlers}.

% connect

connect(Connection) ->
    gen_tcp:connect(Connection#connection.hostname, ?REMOTE_PORT, [
        binary,
        {active, true},
        {send_timeout, 2000}
    ]).

% tcp_send

tcp_send(State, Data) ->
    gen_tcp:send(State#state.socket, Data),
    State.

% request

request_with_handler(State, Operation) -> request_with_handler(State, Operation, []).
request_with_handler(State, Operation, Parameters) ->
    NewState = request(State, Operation, Parameters),
    register_response_handler(NewState, Operation).

request(State, Operation) -> request(State, Operation, []).
request(State, Operation, Parameters) ->
    Flags = 0,
    IsReply = 0,
    ErrorCode = 0,
    
    OperationCode = hotline_constants:transaction_to_code(Operation),
    TransactionId = State#state.transaction_id + 1,
    
    ?LOG("SND [~p:~p] ~p", [TransactionId, Operation, Parameters]),
    
    % Built parameter data
    ParameterData = lists:map(fun ({ParamK, ParamV}) ->
        Type = hotline_constants:field_to_code(ParamK),
        case ParamV of
            ParamV when is_integer(ParamV) ->
                Size = 4,
                Binary = <<ParamV:32>>;
            ParamV when is_list(ParamV) ->
                Binary = list_to_binary(ParamV),
                Size = size(Binary);
            ParamV when is_binary(ParamV) ->
                Binary = ParamV,
                Size = size(Binary)
        end,
        <<Type:16,Size:16,Binary/binary>>
    end, Parameters),
    
    ParameterCount = length(Parameters),
    Data = [<<ParameterCount:16>>, ParameterData],
    
    TotalSize = iolist_size(Data),
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
    tcp_send(State#state{transaction_id=TransactionId}, [Header, Data]).

% transactions_parse

transactions_parse(Packet) ->
    {Transactions, Rest} = transactions_parse(Packet, []),
    {lists:reverse(Transactions), Rest}.
transactions_parse(<<>>, Transactions) -> {Transactions, <<>>};
transactions_parse(<<
        Flags:8,
        IsReply:8,
        OperationCode:16,
        TransactionId:32,
        ErrorCode:32,
        _TotalSize:32,
        DataSize:32,
        DataPart:DataSize/binary,
        Rest/binary
    >>, Transactions) ->
    <<_ParameterCount:16,ParameterData/binary>> = DataPart,
    transactions_parse(Rest, [#transaction{
        flags=Flags,
        is_reply=IsReply,
        operation=hotline_constants:transaction_to_atom(OperationCode),
        id=TransactionId,
        error_code=ErrorCode,
        parameters=params_parse(ParameterData)
    } | Transactions]);
transactions_parse(Packet, Transactions) -> {Transactions, Packet}.

% params_parse

params_parse(Data) -> lists:reverse(params_parse(Data, [])).
params_parse(<<>>, Acc) -> Acc;
params_parse(<<
    FieldType:16,
    FieldSize:16,
    FieldData:FieldSize/binary,
    Rest/binary
    >>, Acc) ->
    params_parse(Rest, [{hotline_constants:field_to_atom(FieldType), FieldData}|Acc]).

% handshake

handshake(State) ->
    Version = 1,
    SubVersion = 2,
    tcp_send(State, <<"TRTP","HOTL",Version:16,SubVersion:16>>).

% requests

login(State) ->
    Connection = State#state.connection,
    Params = [
        {user_login, Connection#connection.username},
        {user_password, Connection#connection.password},
        {user_name, list_to_binary(Connection#connection.name)},
        {user_icon_id, Connection#connection.icon}
    ],
    NewState = request_with_handler(State, login, Params),
    websocket(State, [
        {type, <<"login">>}
    ] ++ Params),
    NewState#state{status=login}.

get_user_name_list(State) ->
    websocket(State, [{type, <<"get_user_name_list">>}]),
    request_with_handler(State, get_user_name_list).

chat_send(State, Line) ->
    request(State, chat_send, [
        {data, Line},
        {chat_options, 0}
    ]).

% send_user_list

send_user_list(State) ->
    websocket(State, [
        {type, <<"user_name_list">>},
        {userlist, [[
            {id, UserId},
            {nick, User#user.nick},
            {icon, User#user.icon},
            {status, User#user.status}
        ] || {UserId, User} <- State#state.user_list]}
    ]),
    State.

% tcp handlers

tcp(State = #state{status=connecting}, <<"TRTP",0,0,0,0>>) ->
    login(State);

tcp(_State = #state{status=connecting}, <<"TRTP",Error:32>>) ->
    exit({handshake_error, Error});

tcp(State, Packet) ->
    PacketBuffer = State#state.packet_buffer,
    {Transactions, Rest} = transactions_parse(<<PacketBuffer/binary,Packet/binary>>),
    NewState = State#state{packet_buffer=Rest},
    lists:foldl(fun (Transaction, CurrentState) ->
        transaction(CurrentState, Transaction)
    end, NewState, Transactions).

% response handlers

response(State, login, _Transaction) ->
    get_user_name_list(State#state{status=connected});
    
response(State, get_user_name_list, Transaction) ->
    ChatSubject = proplists:get_value(chat_subject, Transaction#transaction.parameters),
    UserList = [
        {UserId, #user{
            id=UserId,
            nick=Nick,
            icon=Icon,
            status=Status
        }}
        || {Type, <<
            UserId:16,
            Icon:16,
            Status:16,
            NickSize:16,
            Nick:NickSize/binary
        >>} <- Transaction#transaction.parameters,
        Type =:= user_name_with_info
    ],
    ?LOG("~p", [UserList]),
    send_user_list(State#state{chat_subject=ChatSubject, user_list=UserList});

response(State, Type, Transaction) ->
    ?LOG("RSP [~p:~p] ~p", [Transaction#transaction.id, Type, Transaction]),
    State.

% transaction handlers

transaction(State, Transaction = #transaction{operation=chat_msg}) ->
    Message = binary_to_list(proplists:get_value(data, Transaction#transaction.parameters)),
    StrippedMessage = string:strip(Message, left, $\r),
    ?LOG("~s", [StrippedMessage]),
    websocket(State, [
        {type, <<"chat_msg">>},
        {msg, list_to_binary(StrippedMessage)}
    ]),
    State;

transaction(State, Transaction = #transaction{operation=notify_change_user}) ->
    % Construct a user record
    Params = Transaction#transaction.parameters,
    <<UserId:16>> = proplists:get_value(user_id, Params),
    <<Icon:16>> = proplists:get_value(user_icon_id, Params),
    <<Status:16>> = proplists:get_value(user_flags, Params),
    User = #user{
        id=UserId,
        nick=proplists:get_value(user_name, Params),
        icon=Icon,
        status=Status
    },
    % Replace or append the user in the user_list
    UserList = State#state.user_list,
    NewUser = {UserId, User},
    case proplists:get_value(UserId, UserList) of
        undefined ->
            % Add user
            NewList = lists:append(UserList, [NewUser]);
        _StateUser ->
            % Modify user
            NewList = lists:keyreplace(UserId, 1, UserList, NewUser)
    end,
    send_user_list(State#state{user_list=NewList});

transaction(State, Transaction = #transaction{operation=notify_delete_user}) ->
    % Remove from user_list
    <<UserId:16>> = proplists:get_value(user_id, Transaction#transaction.parameters),
    NewList = proplists:delete(UserId, State#state.user_list),
    send_user_list(State#state{user_list=NewList});

transaction(State, Transaction) ->
    % Check for a transaction handler
    TxnId = Transaction#transaction.id,
    case proplists:get_value(TxnId, State#state.response_handlers) of
        undefined ->
            % No response handler
            ?LOG("RCV [~p:~p] ~p", [TxnId, Transaction#transaction.operation, Transaction]),
            State;
        Type ->
            % Remove response handler and call
            ResponseHandlers = proplists:delete(TxnId, State#state.response_handlers),
            NewState = State#state{response_handlers=ResponseHandlers},
            response(NewState, Type, Transaction)
    end.
