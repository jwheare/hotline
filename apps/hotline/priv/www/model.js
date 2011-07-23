var MODEL = {};
(function () {
    MODEL.News = Backbone.Model.extend({
        spec: [
            'messages'
        ]
    });
    MODEL.User = Backbone.Model.extend({
        spec: [
            'id',
            'nick',
            'icon',
            'status'
        ]
    });
    MODEL.Message = Backbone.Model.extend({
        spec: [
            'from_id',
            'from',
            'msg'
        ]
    });
    MODEL.Line = Backbone.Model.extend({
        spec: [
            'type',
            'time'
        ]
    });
    var LineSpec = {
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
        user_nick_change: [
            'user',
            'old_nick'
        ]
    };
})();
