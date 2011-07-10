-module(hotline_replay).

-export([run/2]).

-include("hotline.hrl").

run(_Handshake, Messages) ->
    lists:foldl(fun ({Direction, Packet}, PacketBuffer) ->
        {Transactions, RestPacket} = hotline_c2s:transactions_parse(<<PacketBuffer/binary,Packet/binary>>),
        lists:foreach(
            fun (Transaction) ->
                log(case Direction of snd -> "->"; rcv -> " <-" end, Transaction)
            end,
            Transactions
        ),
        RestPacket
    end, <<>>, Messages).
    
log(Direction, Transaction) ->
    io:format("~s [~p] ~p: ~p~n", [
        Direction,
        Transaction#transaction.id,
        Transaction#transaction.operation,
        Transaction
    ]).
