-module(hotline_c2s).

-behaviour(gen_server).

-export([
    start_link/0,
    stop/0,
    
    register_websocket/1,
    
    transactions_parse/1,
    
    chat_send/1,
    chat_send/2,
    send_instant_msg/2,
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
-define(CONFVAL(K, Default), case application:get_env(hotline, K) of
    undefined -> Default;
    {ok, Value}     -> Value 
end).
-define(CONFVAL(K), ?CONFVAL(K, undefined)).

-record(state, {
    socket,                    % tcp socket
    status=disconnected,       % connected status
    connection,                % connection object
    user_id,                   % user id once logged in
    access_level,              % 64-bit access level mask
    version,                   % server version once logged in
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

chat_send(Line) when is_binary(Line) ->
    chat_send(Line, false).
chat_send(Line, Emote) when is_binary(Line), is_boolean(Emote) ->
    gen_server:call(?MODULE, {chat_send, Line, Emote}).

send_instant_msg(UserId, Message) when is_integer(UserId),
                                       is_binary(Message),
                                       Message =/= <<>> ->
    gen_server:call(?MODULE, {send_instant_msg, UserId, Message}).

change_nick(Nick) when is_binary(Nick), Nick =/= <<>> ->
    gen_server:call(?MODULE, {change_nick, Nick}).

change_icon(Icon) when is_integer(Icon) ->
    gen_server:call(?MODULE, {change_icon, Icon}).

get_state() ->
    gen_server:call(?MODULE, get_state).
    
register_websocket(Pid) ->
    gen_server:cast(?MODULE, {register_websocket, Pid}).

% gen_server callbacks

init([]) ->
    Connection = #connection{
        hostname = ?CONFVAL(connection_hostname, ?CONFVAL(connection_default_hostname)),
        title    = ?CONFVAL(connection_title, ?CONFVAL(connection_default_title)),
        name     = ?CONFVAL(connection_name, ?CONFVAL(connection_default_name)),
        username = ?CONFVAL(connection_username, ?CONFVAL(connection_default_username)),
        password = ?CONFVAL(connection_password, ?CONFVAL(connection_default_password)),
        icon     = ?CONFVAL(connection_icon, ?CONFVAL(connection_default_icon))
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

handle_call({chat_send, Line, Emote}, _From, State) ->
    NewState = chat_send(State, Line, Emote),
    {reply, ok, NewState};

handle_call({send_instant_msg, UserId, Message}, _From, State) ->
    NewState = send_instant_msg(State, UserId, Message),
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
    % Monitor websocket so we can unregister it when it goes down
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
        {keepalive, true},
        {nodelay, true},
        {reuseaddr, true}
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
    % Update status
    NewState = State#state{status=login},
    
    % Send login details
    Connection = NewState#state.connection,
    NewState2 = request_with_handler(NewState, login, [
        {user_login, encode_binary_string(Connection#connection.username)},
        {user_password, encode_binary_string(Connection#connection.password)},
        {user_name, Connection#connection.name},
        {user_icon_id, Connection#connection.icon},
        {vers, ?SERVER_VERSION}
    ]),
    
    % Send ws messages
    ws(NewState2, [
        {type, <<"login">>},
        {login, Connection#connection.username},
        {username, Connection#connection.name},
        {icon, Connection#connection.icon}
    ]).

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

send_instant_msg(State, UserId, Message) ->
    request(State, send_instant_msg, [
        {user_id, UserId},
        {options, hotline_constants:opt_to_code(user_message)},
        {data, Message}
    ]).

chat_send(State, Line, Emote) ->
    case Line of
        <<"/nick ", Nick/binary>> ->
            % Change nick
            change_nick(State, Nick);
        <<>> ->
            % Empty line, do nothing
            State;
        _ ->
            % Send chat, optionally as an emote
            Options = case Emote of
                true -> 1;
                _    -> 0
            end,
            request(State, chat_send, [
                {data, Line},
                {chat_options, Options}
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

% user list management

add_user(State, User) ->
    % Update state
    NewList = lists:append(State#state.user_list, [{User#user.id, User}]),
    NewState = State#state{user_list=NewList},
    
    % Send ws messages
    ws(NewState, [
        {type, <<"user_joined">>},
        {user, user_to_proplist(User)}
    ]).

modify_user(State, CurrentUser, User) ->
    % Update state
    NewList = lists:keyreplace(User#user.id, 1, State#state.user_list, {User#user.id, User}),
    NewState = State#state{user_list=NewList},
    
    % Send ws messages
    NewState2 = ws(NewState, [
        {type, <<"modify_user">>},
        {user, user_to_proplist(User)}
    ]),
    % Nick changes get a separate message
    case CurrentUser#user.nick =/= User#user.nick of
        true ->
            ws(NewState2, [
                {type, <<"user_nick_change">>},
                {user, user_to_proplist(User)},
                {old_nick, CurrentUser#user.nick}
            ]);
        false ->
            NewState2
    end.

delete_user(State, User) ->
    % Update state
    NewList = proplists:delete(User#user.id, State#state.user_list),
    NewState = State#state{user_list=NewList},
    
    % Send ws messages
    ws(NewState, [
        {type, <<"user_left">>},
        {user, user_to_proplist(User)}
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

response(State, login, Transaction) ->
    Params = Transaction#transaction.parameters,
    case proplists:get_value(error_text, Params) of
        undefined ->
            % Update state
            NewState = State#state{status=connected},
            
            WsProps = [
                {type, <<"logged_in">>}
            ],
            
            % Conditionally set user_id
            {WsProps2, NewState2} = case proplists:get_value(user_id, Params) of
                <<UserId:16>> ->
                    {WsProps ++ [
                        {user_id, UserId}
                    ], NewState#state{user_id=UserId}};
                _ ->
                    {WsProps, NewState}
            end,
            
            % Conditionally set version id
            {WsProps3, NewState3} = case proplists:get_value(vers, Params) of
                <<Version:16>> ->
                    {WsProps2 ++ [
                        {version, Version}
                    ], NewState2#state{version=Version}};
                _ ->
                    {WsProps2, NewState2}
            end,
            
            % Send ws message
            NewState4 = ws(NewState3, WsProps3),
            
            % Final connection initialisation
            NewState5 = set_client_user_info(NewState4),
            NewState6 = get_user_name_list(NewState5),
            NewState7 = get_messages(NewState6),
            NewState7;
        ErrorText ->
            % Error logging in
            NewState = ws(State, [
                {type, <<"login_error">>},
                {msg, ErrorText}
            ]),
            
            % Terminate socket
            terminate({login_error, ErrorText}, NewState)
    end;

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
    NewState = State#state{chat_subject=ChatSubject, user_list=UserList},
    ws(NewState, [
        {type, <<"user_name_list">>},
        {chat_subject, NewState#state.chat_subject},
        {userlist, [user_to_proplist(User) || {_UserId, User} <- NewState#state.user_list]}
    ]);

response(State, get_msgs, Transaction) ->
    Messages = proplists:get_value(data, Transaction#transaction.parameters),
    ws(State, [
        {type, <<"get_msgs">>},
        {messages, Messages}
    ]);

response(State, _Type, _Transaction) ->
    % No handler
    State.

% transaction handlers

transaction(State, Transaction = #transaction{operation=user_access}) ->
    % Update state
    <<AccessLevel:64>> = proplists:get_value(user_access, Transaction#transaction.parameters),
    NewState = State#state{access_level=AccessLevel},
    
    % Send ws message
    ws(NewState, [
        {type, <<"access_level">>},
        {level, AccessLevel}
    ]);

transaction(State, Transaction = #transaction{operation=chat_msg}) ->
    Message = proplists:get_value(data, Transaction#transaction.parameters),
    ws(State, [
        {type, <<"chat_msg">>},
        {msg, Message}
    ]);

transaction(State, Transaction = #transaction{operation=server_msg}) ->
    <<FromId:16>> = proplists:get_value(user_id,   Transaction#transaction.parameters),
    From          = proplists:get_value(user_name, Transaction#transaction.parameters),
    Message       = proplists:get_value(data,      Transaction#transaction.parameters),
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
    case proplists:get_value(UserId, State#state.user_list) of
        undefined ->
            add_user(State, User);
        CurrentUser ->
            modify_user(State, CurrentUser, User)
    end;

transaction(State, Transaction = #transaction{operation=notify_delete_user}) ->
    % Remove from user_list
    <<UserId:16>> = proplists:get_value(user_id, Transaction#transaction.parameters),
    case proplists:get_value(UserId, State#state.user_list) of
        undefined ->
            % Might have come in before receiving the member list
            State;
        User ->
            delete_user(State, User)
    end;

transaction(State, Transaction = #transaction{operation=disconnect_msg}) ->
    Message = proplists:get_value(data, Transaction#transaction.parameters),
    ?LOG("Kicked: ~s", [Message]),
    NewState = ws(State, [
        {type, <<"kicked">>},
        {msg, Message}
    ]),
    terminate({kicked, Message}, NewState);

transaction(State, Transaction) ->
    % Check for a transaction handler
    TxnId = Transaction#transaction.id,
    case proplists:get_value(TxnId, State#state.response_handlers) of
        undefined ->
            % No response handler
            ?LOG("RCV [~p:~p] ~p", [TxnId, Transaction#transaction.operation, Transaction]),
            State;
        Type ->
            case Transaction#transaction.operation of
                unknown ->
                    ?LOG("RSP [~p:~p] ~p", [Transaction#transaction.id, Type, Transaction]);
                Operation ->
                    ?LOG("RSP-OP [~p(~p):~p] ~p", [Transaction#transaction.id, Type, Operation, Transaction])
            end,
            % Remove response handler and call
            ResponseHandlers = proplists:delete(TxnId, State#state.response_handlers),
            NewState = State#state{response_handlers=ResponseHandlers},
            response(NewState, Type, Transaction)
    end.
