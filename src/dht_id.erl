%%
%% dht_id.erl
%% Kevin Lynx
%% 06.05.2013
%%
-module(dht_id).
-export([random/0,
         distance/2,
         min/0,
         max/0,
         id_cmp/2,
         integer_id/1,
         list_id/1,
         tohex/1]).

random() ->
    Byte  = fun() -> random:uniform(256) - 1 end,
    Bytes = [Byte() || _ <- lists:seq(1, 20)],
    integer_id(list_to_binary(Bytes)).

distance(BID0, BID1) when is_binary(BID0), is_binary(BID1) ->
    <<ID0:160>> = BID0,
    <<ID1:160>> = BID1,
    ID0 bxor ID1;

distance(ID0, ID1) when is_integer(ID0), is_integer(ID1) ->
    ID0 bxor ID1.

min() ->
    0.

max() ->
    integer_id(list_to_binary([255 || _ <- lists:seq(1, 20)])).

id_cmp(ID1, ID2) when is_binary(ID1) and is_binary(ID2) ->
    if 
        ID1 < ID2 -> -1;
        ID1 > ID2 -> 1;
        true -> 0
    end.

integer_id(BID) when is_binary(BID) ->
    <<ID:160>> = BID,
    ID.

list_id(ID) ->
    <<ID:160>>.

tohex(ID) when is_binary(ID) ->
    lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(ID)]);

tohex(ID) ->
    tohex(list_id(ID)).


