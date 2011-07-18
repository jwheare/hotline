var HOTLINE = {};
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
function writeScroll (message, text, row) {
    var scroll = $('#scroll');
    var scrollBottom = scroll.scrollTop() + scroll.height();
    var scrolledFromBottom = scroll.prop('scrollHeight') - scrollBottom;
    
    if (!row) {
        row = $('<div>');
        scroll.append(row);
    }
    var date = new Date(message.time * 1000);
    var timestamp = $('<span>')
        .attr('title', date.toString('dddd, MMMM dd, yyyy HH:mm:ss'))
        .addClass('timestamp')
        .text(date.toString('HH:mm'));
    row
        .addClass('type_' + message.type)
        .text(text)
        .data('message', message)
        .prepend('<span class="g">] </span>')
        .prepend(timestamp)
        .prepend(' <span class="g">[</span>');
    
    // Keep scrolled to bottom
    if (scrolledFromBottom == 0) {
        scroll.scrollTop(scroll.prop('scrollHeight'));
    }
}

function getLastScroll () {
    return $('#scroll div:last-child');
}

var User = Backbone.Model.extend({});
var UserView = Backbone.View.extend({
    tagName: 'li',
    render: function () {
        return this.renderNick().renderIcon();
    },
    
    nick: function () {
        return this.model.get('nick');
    },
    renderNick: function () {
        $(this.el).text(this.nick());
        return this;
    },
    renderIcon: function () {
        $(this.el).css('background-image', 'url(/icons/' + this.model.get('icon') + '.gif)');
        return this;
    },
    joinedMessage: function () {
        return '→ ' + this.nick() + ' joined';
    },
    leftMessage: function () {
        return '← ' + this.nick() + ' left';
    },
    nickChangeMessage: function (oldNick) {
        return oldNick + ' → ' + this.nick();
    },
    joinedNickChangeMessage: function (oldNick) {
        return '→ ' + this.nickChangeMessage(oldNick) + ' joined';
    }
});


var UserList = Backbone.Collection.extend({
    model: User,
    url: '/users/'
});
var UserListView = Backbone.View.extend({
    el: 'ul#members',
    
    initialize: function () {
        this.collection.bind("reset", $.proxy(this, "render"));
    },
    addUser: function (user) {
        var userView = new UserView({
            model: user,
            className: 'status_' + user.get('status')
        });
        $(this.el).append(userView.render().el);
    },
    render: function () {
        $(this.el).empty();
        this.collection.each($.proxy(this, "addUser"));
        return this;
    }
});

HOTLINE.memberList = new UserList();
new UserListView({
    collection: HOTLINE.memberList
});

var messageHandlers = {
    idle: function () {
    },
    handshake: function (message) {
        $('title').text(message.hostname + ' | Hotline');
        $('#title').text(message.title);
        $('#hostname').text(message.hostname);
        $('#status').text('Handshaking…');
    },
    login: function (message) {
        $('#status').text('Handshaking…');
    },
    logged_in: function (message) {
        $('#status').text('');
    },
    chat_msg: function (message) {
        writeScroll(message, message.msg);
    },
    server_msg: function (message) {
        writeScroll(message, '[' + message.from + '] ' + message.msg);
        var fromId = 'message_' + message.from_id;
        if ($('#'+fromId)[0]) {
            $('#'+fromId).find('.count').text();
        }
    },
    get_msgs: function (message) {
        $('#news').text(message.messages.replace(/\r\n/g, '\n').replace(/\r/g, '\n'));
    },
    socket_closed: function (message) {
        $('#status').text("Disconnected");
        writeScroll(message, "Disconnected");
    },
    user_joined: function (message) {
        var userView = new UserView({model: new User(message.user)});
        writeScroll(message, userView.joinedMessage());
    },
    user_left: function (message) {
        var userView = new UserView({model: new User(message.user)});
        writeScroll(message, userView.leftMessage());
    },
    user_nick_change: function (message) {
        var user = new User(message.user);
        var userView = new UserView({model: user});
        var lastScroll = getLastScroll();
        var lastMessage = lastScroll.data('message');
        if (lastMessage && lastMessage.type == 'user_joined') {
            var lastUser = new User(lastMessage.user);
            if (user.id == lastUser.id) {
                writeScroll(message, userView.joinedNickChangeMessage(message.old_nick), lastScroll);
                return;
            }
        }
        writeScroll(message, userView.nickChangeMessage(message.old_nick));
    },
    user_name_list: function (message) {
        HOTLINE.memberList.reset(message.userlist);
    }
};

// Message handlers
function handle_message (message) {
    if (message.type in messageHandlers) {
        messageHandlers[message.type](message);
    } else {
        console.log(event.data);
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

function showNews () {
    $('#chatLink').removeClass('active');
    $('#newsLink').addClass('active');
    $('#news').show();
    $('#chat').hide();
}

function showChat () {
    $('#chatLink').addClass('active');
    $('#newsLink').removeClass('active');
    $('#news').hide();
    $('#chat').show();
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
SOCKET = initSocket();

})();