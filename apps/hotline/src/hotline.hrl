-record(connection, {
    hostname,
    title,
    username,
    password,
    name,
    icon
}).
-record(transaction, {
    flags,
    is_reply,
    operation,
    id,
    error_code,
    parameters
}).
