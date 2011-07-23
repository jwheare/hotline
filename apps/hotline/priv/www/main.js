var APP = {};
(function () {

// Init socket
function startSocket() {
    var ws = new WebSocket("ws://" + window.location.host);
    ws.onopen = function (event) {
        console.info('WebSocket open', event);
    };
    ws.onmessage = function (event) {
        try {
            var message = JSON.parse(event.data);
        } catch (exception) {
            console.error('invalid JSON', event.data, exception);
            return;
        }
        handle_message(message);
    };
    ws.onclose = function (event) {
        console.info('WebSocket closed', event);
    };
    ws.onerror = function (event) {
        console.warn('WebSocket error', event);
    };
    return ws;
}

APP.memberList = new COLLECTION.UserList();
new VIEW.UserListView({
    collection: APP.memberList
});
APP.conversationList = new COLLECTION.Conversations();
new VIEW.ConversationsView({
    collection: APP.conversationList
});
APP.lines = new COLLECTION.Lines();
new VIEW.LinesView({
    collection: APP.lines
});
APP.news = new MODEL.News();
new VIEW.NewsView({
    model: APP.news
});
APP.connection = new MODEL.Connection();
new VIEW.ConnectionView({
    model: APP.connection
});

// Message handlers
function handle_message (message) {
    if (message.type in MESSAGE) {
        MESSAGE[message.type](message);
    } else {
        console.log(event.data);
    }
}

var MESSAGE = {
    idle: function () {
    },
    handshake: function (message) {
        APP.connection.set({
            state: 'handshaking',
            title: message.title,
            hostname: message.hostname
        });
    },
    login: function (message) {
        APP.connection.set({state: 'loggingIn'});
    },
    logged_in: function (message) {
        APP.connection.set({state: 'loggedIn'});
    },
    chat_msg: function (message) {
        APP.lines.add(message);
    },
    server_msg: function (message) {
        APP.conversationList.add(message);
        APP.lines.add(message);
    },
    get_msgs: function (message) {
        APP.news.set(message);
    },
    socket_closed: function (message) {
        APP.connection.set({state: 'disconnected'});
        APP.lines.add(message);
    },
    user_joined: function (message) {
        APP.lines.add(message);
    },
    user_left: function (message) {
        APP.lines.add(message);
    },
    user_nick_change: function (message) {
        APP.lines.add(message);
    },
    user_name_list: function (message) {
        APP.memberList.reset(message.userlist);
    }
};

// Socket API methods
function send (message) {
    SOCKET.send(JSON.stringify(message));
}
function chat (text, emote) {
    send({
        "type": "chat_send",
        "msg": text,
        "emote": emote
    });
}

function showNews () {
    $('#chatLink').removeClass('active');
    $('#newsLink').addClass('active');
    $('#news').show();
    $('#chat').addClass('hidden');
}

function showChat () {
    $('#chatLink').addClass('active');
    $('#newsLink').removeClass('active');
    $('#news').hide();
    $('#chat').removeClass('hidden');
}

$(window).keydown(function (e) {
    if (!$(e.target).is('input, textarea')) {
        if (!e.metaKey && !e.ctrlKey && (e.keyCode === 0 || e.keyCode >= 48)) {
            // Focus chat box
            $('#inputBox').focus();
        }
    }
    return true;
});

// Input key handler
$('#inputBox')
    .keydown(function (e) {
        switch (e.keyCode) {
        case 13: // RETURN
            e.preventDefault();
            // Send
            chat($(this).val(), (e.metaKey || e.ctrlKey));
            // Clear
            $(this).val('');
            break;
        }
    })
    .focus();

$('#newsLink')
    .click(function (e) {
        e.preventDefault();
        showNews();
    });

$('#chatLink')
    .click(function (e) {
        e.preventDefault();
        showChat();
    });

// GO!
SOCKET = startSocket();

})();