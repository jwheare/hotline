(function () {

// Init socket
function initSocket() {
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

// Write to scroll
function writeScroll (message, text) {
    var scroll = $('#scroll');
    var scrollBottom = scroll.scrollTop() + scroll.height();
    var scrolledFromBottom = scroll.prop('scrollHeight') - scrollBottom;
    scroll.append($('<div>').addClass(message.type).text(text));
    // Keep scrolled to bottom
    if (scrolledFromBottom == 0) {
        scroll.scrollTop(scroll.prop('scrollHeight'));
    }
}

// Message handlers
function handle_message (message) {
    switch (message.type) {
    case "idle":
        // Just to keep the socket alive
        break;
    case "handshake":
        $('title').text(message.hostname + ' | Hotline');
        writeScroll(message, "Handshaking…");
        break;
    case "login":
        writeScroll(message, "Logging in…");
        break;
    case "logged_in":
        writeScroll(message, "Logged in.");
        break;
    case "chat_msg":
        writeScroll(message, message.msg);
        break;
    case "server_msg":
        writeScroll(message, '[' + message.from + '] ' + message.msg);
        break;
    case "get_msgs":
        $('#news').text(message.messages.replace(/\r\n/g, '\n').replace(/\r/g, '\n'));
        break;
    case "socket_closed":
        writeScroll(message, "Disconnected");
        break;
    case "user_joined":
        writeScroll(message,  '→ ' + message.user.nick + ' joined');
        break;
    case "user_left":
        writeScroll(message,  '← ' + message.user.nick + ' left');
        break;
    case "user_nick_change":
        writeScroll(message, message.old_nick + ' → ' + message.user.nick);
        break;
    case "user_name_list":
        var memberList = $('<ul>');
        $.each(message.userlist, function (i, user) {
            var member = $('<li>')
                .text(user.nick)
                .attr('id', 'user_' + user.id)
                .addClass('status_' + user.status)
                .data('icon', user.icon)
                .css('background-image', 'url(/icons/'+user.icon+'.gif)');
            memberList.append(member);
        });
        $('#members').html(memberList);
        break;
    default:
        console.log(event.data);
        break;
    }
}

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

// GO!
SOCKET = initSocket();

})();