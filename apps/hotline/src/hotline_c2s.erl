-module(hotline_c2s).

-behaviour(gen_server).

-export([
    start_link/0,
    stop/0,
    
    register_websocket/1,
    
    transactions_parse/1,
    
    chat_send/1,
    change_nick/1,
    change_icon/1,
    
    get_state/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("hotline.hrl").

-define(REMOTE_PORT, 5500).
-define(SERVER_VERSION, 185).
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
    websockets = [],           % pids to send socket data to
    backlog = []               % event backlog for the active session
}).

% Public methods

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

chat_send(Line) ->
    gen_server:call(?MODULE, {chat_send, Line}).

change_nick(Nick) ->
    gen_server:call(?MODULE, {change_nick, Nick}).

change_icon(Icon) ->
    gen_server:call(?MODULE, {change_icon, Icon}).

get_state() ->
    gen_server:call(?MODULE, get_state).
    
register_websocket(Pid) ->
    gen_server:cast(?MODULE, {register_websocket, Pid}).

% gen_server callbacks

init([]) ->
    Connection = #connection{
        hostname = <<"livebus.org">>,
        title    = <<"Livebus">>,
        name     = <<"Erlanger">>,
        icon     = 150
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
    NewState = ws(State, [
        {type, <<"socket_closed">>}
    ]),
    gen_tcp:close(NewState#state.socket),
    NewState2 = NewState#state{
        socket = undefined,
        status = disconnected,
        connection = undefined,
        transaction_id = 0,
        response_handlers = [],
        packet_buffer = <<>>,
        user_list = [],
        chat_subject = undefined
    },
    {stop, Reason, NewState2}.

code_change(_PreviousVersion, State, _Extra) ->
    {ok, State}.


% time functions

epoch () ->
    now_to_seconds(now()).

now_to_seconds({MegaSecs, Secs, _MicroSecs}) ->
    (MegaSecs*1000000) + Secs.

% ws

ws(State, Message) ->
    MessageWithTime = Message ++ [ {time, epoch()} ],
    lists:foreach(fun (WebSocket) ->
        WebSocket ! {hotline_message, MessageWithTime}
    end, State#state.websockets),
    % Append to backlog
    Backlog = lists:append(State#state.backlog, [MessageWithTime]),
    State#state{backlog=Backlog}.

% handle_call

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({chat_send, Line}, _From, State) ->
    NewState = chat_send(State, Line),
    {reply, ok, NewState};

handle_call({change_nick, Nick}, _From, State) ->
    NewState = change_nick(State, Nick),
    {reply, ok, NewState};

handle_call({change_icon, Icon}, _From, State) ->
    NewState = change_icon(State, Icon),
    {reply, ok, NewState};

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% handle_cast

handle_cast({register_websocket, Pid}, State) ->
    % Monitory for unregistering
    monitor(process, Pid),
    % Send backlog
    lists:foreach(fun (Message) ->
        Pid ! {hotline_message, Message}
    end, State#state.backlog),
    % Register
    WebSockets = [Pid | State#state.websockets],
    {noreply, State#state{websockets=WebSockets}};
    
handle_cast(_Request, State) ->
    {noreply, State}.

% handle_info

handle_info(connected, State) ->
    NewState = ws(State, [
        {type, <<"handshake">>},
        {hostname, State#state.connection#connection.hostname},
        {title, State#state.connection#connection.title}
    ]),
    handshake(NewState),
    {noreply, NewState};

handle_info({tcp, _Socket, Packet}, State) ->
    NewState = tcp(State, Packet),
    {noreply, NewState};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, tcp_closed, State};

handle_info({tcp_error, _Socket, Reason}, State) ->
    {stop, {tcp_error, Reason}, State};

handle_info({'DOWN', _MonitorRef, _Type, Pid, _Info}, State) ->
    WebSockets = lists:delete(Pid, State#state.websockets),
    {noreply, State#state{websockets=WebSockets}};

handle_info(Request, State) ->
    ?LOG("Unhandled request: ~p", [Request]),
    {noreply, State}.

% register_response_handler

register_response_handler(State, Type) ->
    ResponseHandlers = [{State#state.transaction_id, Type} | State#state.response_handlers],
    State#state{response_handlers=ResponseHandlers}.

% connect

connect(Connection) ->
    gen_tcp:connect(binary_to_list(Connection#connection.hostname), ?REMOTE_PORT, [
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
    params_parse(Rest, [{hotline_constants:field_to_atom(FieldType), FieldData} | Acc]).

% encode

encode_binary_string(BinaryString) ->
    [Char bxor 16#FF || <<Char>> <= BinaryString].

% handshake

handshake(State) ->
    Version = 1,
    SubVersion = 2,
    ?LOG("Handshaking ~p", [State#state.connection#connection.hostname]),
    tcp_send(State, <<"TRTP","HOTL",Version:16,SubVersion:16>>).

% requests

login(State) ->
    Connection = State#state.connection,
    Params = [
        {user_login, encode_binary_string(Connection#connection.username)},
        {user_password, encode_binary_string(Connection#connection.password)},
        {user_name, Connection#connection.name},
        {user_icon_id, Connection#connection.icon},
        {vers, ?SERVER_VERSION}
    ],
    NewState = request_with_handler(State, login, Params),
    NewState2 = ws(NewState, [
        {type, <<"login">>},
        {login, Connection#connection.username},
        {username, Connection#connection.name},
        {icon, Connection#connection.icon}
    ]),
    NewState2#state{status=login}.

change_nick(State, Nick) ->
    Connection = State#state.connection#connection{name=Nick},
    set_client_user_info(State#state{connection=Connection}).

change_icon(State, Icon) ->
    Connection = State#state.connection#connection{icon=Icon},
    set_client_user_info(State#state{connection=Connection}).

set_client_user_info(State) ->
    request(State, set_client_user_info, [
        {user_name, State#state.connection#connection.name},
        {user_icon_id, State#state.connection#connection.icon}
    ]).

get_user_name_list(State) ->
    request_with_handler(State, get_user_name_list).

get_messages(State) ->
    request_with_handler(State, get_msgs).

chat_send(State, Line) ->
    case Line of
        <<"/nick ", Nick/binary>> ->
            change_nick(State, Nick);
        <<>> ->
            State;
        _ ->
            request(State, chat_send, [
                {data, Line},
                {chat_options, 0}
            ])
    end.

% build_user_object

user_to_proplist(User) ->
    [
        {id, User#user.id},
        {nick, User#user.nick},
        {icon, User#user.icon},
        {status, User#user.status}
    ].

% send_user_list

send_user_list(State) ->
    ws(State, [
        {type, <<"user_name_list">>},
        {userlist, [user_to_proplist(User) || {_UserId, User} <- State#state.user_list]}
    ]).

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
    NewState = State#state{status=connected},
    NewState2 = set_client_user_info(NewState),
    NewState3 = ws(NewState2, [
        {type, <<"logged_in">>}
    ]),
    NewState4 = get_user_name_list(NewState3),
    NewState5 = get_messages(NewState4),
    NewState5;
    
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

response(State, get_msgs, Transaction) ->
    Messages = proplists:get_value(data, Transaction#transaction.parameters),
    ws(State, [
        {type, <<"get_msgs">>},
        {messages, Messages}
    ]);

response(State, Type, Transaction) ->
    ?LOG("RSP [~p:~p] ~p", [Transaction#transaction.id, Type, Transaction]),
    State.

% transaction handlers

transaction(State, Transaction = #transaction{operation=chat_msg}) ->
    Message = proplists:get_value(data, Transaction#transaction.parameters),
    ?LOG("~s", [Message]),
    ws(State, [
        {type, <<"chat_msg">>},
        {msg, Message}
    ]);

transaction(State, Transaction = #transaction{operation=server_msg}) ->
    <<FromId:16>> = proplists:get_value(user_id,   Transaction#transaction.parameters),
    From          = proplists:get_value(user_name, Transaction#transaction.parameters),
    Message       = proplists:get_value(data,      Transaction#transaction.parameters),
    ?LOG("PM [~s] ~s", [From, Message]),
    ws(State, [
        {type, <<"server_msg">>},
        {from_id, FromId},
        {from, From},
        {msg, Message}
    ]);

transaction(State, Transaction = #transaction{operation=invite_to_chat}) ->
    <<ChatId:32>> = proplists:get_value(chat_id,   Transaction#transaction.parameters),
    <<FromId:16>> = proplists:get_value(user_id,   Transaction#transaction.parameters),
    From          = proplists:get_value(user_name, Transaction#transaction.parameters),
    ?LOG("Invite [~s] ~B", [From, ChatId]),
    ws(State, [
        {type, <<"invite_to_chat">>},
        {chat_id, ChatId},
        {from_id, FromId},
        {from, From}
    ]);

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
            NewList = lists:append(UserList, [NewUser]),
            NewState = ws(State, [
                {type, <<"user_joined">>},
                {user, user_to_proplist(User)}
            ]);
        StateUser ->
            % Modify user
            NewList = lists:keyreplace(UserId, 1, UserList, NewUser),
            NewState = if
                StateUser#user.nick =/= User#user.nick ->
                    ws(State, [
                        {type, <<"user_nick_change">>},
                        {user, user_to_proplist(User)},
                        {old_nick, StateUser#user.nick}
                    ]);
                StateUser#user.status =/= User#user.status ->
                    ws(State, [
                        {type, <<"user_status_change">>},
                        {user, user_to_proplist(User)},
                        {old_status, StateUser#user.status}
                    ]);
                StateUser#user.icon =/= User#user.icon ->
                    ws(State, [
                        {type, <<"user_icon_change">>},
                        {user, user_to_proplist(User)},
                        {old_icon, StateUser#user.icon}
                    ]);
                true -> State
            end
    end,
    send_user_list(NewState#state{user_list=NewList});

transaction(State, Transaction = #transaction{operation=notify_delete_user}) ->
    % Remove from user_list
    <<UserId:16>> = proplists:get_value(user_id, Transaction#transaction.parameters),
    User = proplists:get_value(UserId, State#state.user_list),
    NewList = proplists:delete(UserId, State#state.user_list),
    NewState = State#state{user_list=NewList},
    NewState2 = ws(NewState, [
        {type, <<"user_left">>},
        {user, user_to_proplist(User)}
    ]),
    NewState3 = send_user_list(NewState2),
    NewState3;

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
