-module(hotline_replay).

-export([run/1]).

-include("hotline.hrl").

run(Messages) ->
    lists:foldl(fun ({Direction, Packet}, PacketBuffer) ->
        {Transactions, RestPacket} = hotline_c2s:parse_transactions(<<PacketBuffer/binary,Packet/binary>>),
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
