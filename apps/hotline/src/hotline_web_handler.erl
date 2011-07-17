-module(hotline_web_handler).

-export([message/2]).

to_bool([])     -> false;
to_bool(<<>>)   -> false;
to_bool(0)      -> false;
to_bool(null)   -> false;
to_bool(false)  -> false;
to_bool(_Value) -> true.

message(<<"chat_send">>, Message) ->
    Msg = proplists:get_value(<<"msg">>, Message),
    case proplists:get_value(<<"emote">>, Message) of
        undefined -> hotline_c2s:chat_send(Msg);
        Emote -> hotline_c2s:chat_send(Msg, to_bool(Emote))
    end;

message(<<"change_nick">>, Message) ->
    Nick = proplists:get_value(<<"nick">>, Message),
    hotline_c2s:change_nick(Nick);

message(<<"change_icon">>, Message) ->
    Icon = proplists:get_value(<<"icon">>, Message),
    hotline_c2s:change_icon(Icon).
