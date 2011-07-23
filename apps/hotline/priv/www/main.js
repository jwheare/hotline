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

function nullDefaults (params) {
    var defaults = {};
    $.each(params, function (i, param) {
        defaults[param] = null;
    });
    return defaults;
}

var User = Backbone.Model.extend({
    defaults: nullDefaults([
        "id",
        "nick",
        "icon",
        "status"
    ])
});
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
    renderUser: function (user) {
        var userView = new UserView({
            model: user,
            className: 'status_' + user.get('status')
        });
        $(this.el).append(userView.render().el);
    },
    render: function () {
        $(this.el).empty();
        this.collection.each($.proxy(this, "renderUser"));
        return this;
    }
});

var Message = Backbone.Model.extend({
    defaults: nullDefaults([
        "from_id",
        "from",
        "msg"
    ])
});
var MessageView = Backbone.View.extend({
    getMember: function () {
        return memberList.get(this.model.get('from_id'));
    },
    render: function () {
        
    }
});

var Conversations = Backbone.Collection.extend({
    model: Message,
    url: '/messages/'
});
var ConversationsView = Backbone.View.extend({
    initialize: function () {
        this.collection.bind("add", $.proxy(this, "renderMessage"));
    },
    renderMessage: function (message) {
        var messageView = new MessageView({
            model: message
        });
        messageView.render();
    }
});

var Line = Backbone.Model.extend({});
var Lines = Backbone.Collection.extend({
    model: Line
});
var LineView = Backbone.View.extend({
    tagName: 'div',
    className: 'row',
    defaults: nullDefaults(['type', 'time']),
    initialize: function () {
        this.model.bind("hide", $.proxy(this, 'hide'));
    },
    hide: function () {
        $(this.el).hide();
        return this;
    },
    type: function () {
        return this.model.get('type');
    },
    date: function () {
        return new Date(this.model.get('time') * 1000);
    },
    render: function () {
        $(this.el).addClass('type_' + this.type());
        var date = this.date();
        var timestamp = $('<span>')
            .attr('title', date.toString('dddd, MMMM dd, yyyy HH:mm:ss'))
            .addClass('timestamp')
            .text(date.toString('HH:mm'));
        $(this.el)
            .html(this.renderer())
            .prepend('<span class="g">] </span>')
            .prepend(timestamp)
            .prepend(' <span class="g">[</span>');
        
        return this;
    },
    renderer: function () {
        // defined in sub classes
    }
}, {
    makeRenderer: function (params, renderer) {
        return LineView.extend({
            defaults: function () {
                return this.defaults.extend(nullDefaults(params));
            },
            renderer: renderer
        });
    }
});

var LineViews = {
    chat_msg: LineView.makeRenderer(['msg'], function () {
        return autolink(this.model.get('msg').replace(/^\r/, ''));
    }),
    server_msg: LineView.makeRenderer(['from_id', 'from', 'msg'], function () {
        return '[' + escapeText(this.model.get('from')) + '] ' + autolink(this.model.get('msg'));
    }),
    socket_closed: LineView.makeRenderer([], function () {
        return 'Disconnected';
    }),
    user_joined: LineView.makeRenderer(['user'], function () {
        return '→ ' + escapeText(this.model.get('nick')) + ' joined';
    }),
    user_left: LineView.makeRenderer(['user'], function () {
        return '← ' + escapeText(this.model.get('nick')) + ' left';
    }),
    user_nick_change: LineView.makeRenderer(['user', 'old_nick'], function () {
        return escapeText(this.model.get('old_nick')) + ' → ' + escapeText(this.model.get('user').nick);
    })
};
var ComboLineViews = {
    user_nick_change: function (line, lastLine) {
        // Check predicates
        if (lastLine.get('type') != 'user_joined') {
            return false;
        }
        if (line.get('user').id != lastLine.get('user').id) {
            return false;
        }
        // Return a renderer if the predicates passed
        return LineView.makeRenderer(function () {
            return '→ ' + LineView.user_nick_change.call(this) + ' joined';
        });
    }
};
var LinesView = Backbone.View.extend({
    el: 'div#lines',
    
    initialize: function () {
        this.collection.bind("add", $.proxy(this, "renderLine"));
    },
    renderLine: function (line) {
        var scrolledUp = this.scrolledUpDistance();
        var lastLine = this.collection.last();
        
        var type = line.get('type');
        
        var viewClass;
        if (ComboLineViews[type]) {
            viewClass = ComboLineViews[type](line, lastLine);
        }
        viewClass = viewClass || LineViews[type];
        var lineView = new viewClass({
            model: line
        });
        $(this.el).append(lineView.render().el);
        
        // Keep scrolled to bottom
        if (scrolledUp == 0) {
            this.scrollToBottom();
        }
    },
    
    height: function () {
        return $(this.el).height();
    },
    scrollHeight: function () {
        return $(this.el).prop('scrollHeight');
    },
    viewportTop: function () {
        return $(this.el).scrollTop();
    },
    viewportBottom: function () {
        return this.viewportTop() + this.height();
    },
    scrollTo: function (position) {
        $(this.el).scrollTop(position);
        return this;
    },
    scrolledUpDistance: function () {
        return this.scrollHeight() - this.viewportBottom();
    },
    scrollToBottom: function () {
        this.scrollTo(this.scrollHeight());
        return this;
    }
});

HOTLINE.memberList = new UserList();
new UserListView({
    collection: HOTLINE.memberList
});
HOTLINE.conversationList = new Conversations();
new ConversationsView({
    collection: HOTLINE.conversationList
});
HOTLINE.lines = new Lines();
HOTLINE.linesView = new LinesView({
    collection: HOTLINE.lines
});

function escapeText (text) {
    return text
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/"/g, '&quot;')
        .replace(/'/g, '&apos;');
}
function linkifyCallback (text, href) {
    var html = '';
    if (href) {
        html += _.template('<a href="<%= href %>">', {href : href});
    }
    html += escapeText(text);
    if (href) {
        html += '</a>';
    }
    return html;
}
function autolink (text) {
    return linkify(text, {
        callback: function (cbText, href) {
            var html = '';
            if (href) {
                html += _.template('<a href="<%= href %>" target="_blank">', {href : href});
            }
            html += escapeText(cbText);
            if (href) {
                html += '</a>';
            }
            return html;
        }
    });
}

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
        HOTLINE.lines.add(message);
    },
    server_msg: function (message) {
        HOTLINE.conversationList.add(message);
        HOTLINE.lines.add(message);
    },
    get_msgs: function (message) {
        var messages = message.messages.replace(/\r\n/g, '\n').replace(/\r/g, '\n');
        $('#news').html(autolink(messages));
    },
    socket_closed: function (message) {
        $('#status').text("Disconnected");
        HOTLINE.lines.add(message);
    },
    user_joined: function (message) {
        HOTLINE.lines.add(message);
    },
    user_left: function (message) {
        HOTLINE.lines.add(message);
    },
    user_nick_change: function (message) {
        HOTLINE.lines.add(message);
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
SOCKET = initSocket();

})();