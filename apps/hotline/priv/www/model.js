var MODEL = {};
(function () {
    var UserCollection = Backbone.Collection.extend({
        model: User,
        url: '/users/'
    });
    var MessageCollection = Backbone.Collection.extend({
        model: Message,
        url: '/messages/'
    });
    var LineCollection = Backbone.Collection.extend({
        model: Line
    });
    
    MODEL.Connection = Backbone.Model.extend({
        spec: [
            'state',
            'title',
            'hostname'
        ],
        initialize: function () {
            this.news          = new News();
            this.chat          = new Chat();
            this.lines         = new LineCollection();
            this.conversations = new MessageCollection();
            this.members       = new UserCollection();
            
            // Bind message handlers
            for (var type in this.messageHandlers) {
                this.bind('message:' + type, this.messageHandlers[type]);
            }
            // Log messages with no handlers
            this.bind('all', function (eventName, message) {
                var eventParts = eventName.split(':');
                if (eventParts[0] == 'message' && eventParts[1] in this.messageHandlers !== true) {
                    console.info(message.type, message);
                }
            });
        },
        
        messageHandlers: {
            idle: function (message) {
            },
            handshake: function (message) {
                this.set({
                    state: 'handshaking',
                    title: message.title,
                    hostname: message.hostname
                });
            },
            login: function (message) {
                this.set({state: 'loggingIn'});
            },
            logged_in: function (message) {
                this.set({state: 'loggedIn'});
            },
            chat_msg: function (message) {
                this.addLine(message);
            },
            server_msg: function (message) {
                this.addMessage(message);
                this.addLine(message);
            },
            get_msgs: function (message) {
                this.news.set(message);
            },
            socket_closed: function (message) {
                this.set({state: 'disconnected'});
                this.addLine(message);
            },
            user_joined: function (message) {
                this.addMember(message.user);
                this.addLine(message);
            },
            user_left: function (message) {
                this.removeMember(message.user);
                this.addLine(message);
            },
            modify_user: function (message) {
                this.updateMember(message.user);
            },
            user_nick_change: function (message) {
                // Just print the line, modify_user handles state
                this.addLine(message);
            },
            user_name_list: function (message) {
                this.resetMembers(message.userlist);
            }
        },
        
        addLine: function (line) {
            this.lines.add(line);
        },
        addMessage: function (message) {
            this.conversations.add(message);
        },
        addMember: function (member) {
            this.members.add(member);
        },
        updateMember: function (member) {
            this.members.get(member.id).set(member);
        },
        removeMember: function (member) {
            this.members.remove(member);
        },
        resetMembers: function (members) {
            this.members.reset(members);
        }
    });
    var Chat = Backbone.Model.extend({
        defaults: {
            'type': 'chat_send',
            'msg': '',
            'emote': false
        }
    });
    var News = Backbone.Model.extend({
        spec: [
            'messages'
        ]
    });
    var User = Backbone.Model.extend({
        spec: [
            'id',
            'nick',
            'icon',
            'status'
        ]
    });
    var Message = Backbone.Model.extend({
        spec: [
            'from_id',
            'from',
            'msg'
        ]
    });
    
    var MessageSpec = [
        'type',
        'time'
    ];
    var Line = Backbone.Model.extend({
        spec: MessageSpec
    });
    var MessageSpecs = {
        handshake: [
            'hostname',
            'title'
        ],
        socket_closed: [],
        login: [
            'login',
            'username',
            'icon'
        ],
        logged_in: [],
        get_msgs: [
            'messages'
        ],
        idle: [],
        invite_to_chat: [
            'chat_id',
            'from_id',
            'from'
        ],
        chat_msg: [
            'msg'
        ],
        server_msg: [
            'from_id',
            'from',
            'msg'
        ],
        user_joined: [
            'user'
        ],
        user_left: [
            'user'
        ],
        modify_user: [
            'user',
            'old_user'
        ],
        user_nick_change: [
            'user',
            'old_nick'
        ],
        user_name_list: [
            'chat_subject',
            'userlist'
        ]
    };
})();
