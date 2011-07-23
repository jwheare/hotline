// GO!
APP = {};
APP.connection = new MODEL.Connection();
new VIEW.ConnectionView({
    model: APP.connection
});

SOCKET.start(function handle_message (message) {
    APP.connection.trigger('message:' + message.type, message);
});
