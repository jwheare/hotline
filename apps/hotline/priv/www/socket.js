// Init socket
SOCKET = {};
SOCKET.start = function (messageHandler) {
    SOCKET.ws = new WebSocket("ws://" + window.location.host);
    SOCKET.ws.onopen = function (event) {
        console.info('WebSocket open', event);
    };
    SOCKET.ws.onmessage = function (event) {
        try {
            var message = JSON.parse(event.data);
        } catch (exception) {
            console.error('invalid JSON', event.data, exception);
            return;
        }
        messageHandler(message);
    };
    SOCKET.ws.onclose = function (event) {
        console.info('WebSocket closed', event);
    };
    SOCKET.ws.onerror = function (event) {
        console.warn('WebSocket error', event);
    };
    Backbone.sync = function (method, message) {
        SOCKET.ws.send(JSON.stringify(message));
    };
};
