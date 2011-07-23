var COLLECTION = {};
(function () {
    COLLECTION.UserList = Backbone.Collection.extend({
        model: MODEL.User,
        url: '/users/'
    });
    COLLECTION.Conversations = Backbone.Collection.extend({
        model: MODEL.Message,
        url: '/messages/'
    });
    COLLECTION.Lines = Backbone.Collection.extend({
        model: MODEL.Line
    });
})();
