%%
%% msg.erl
%% Kevin Lynx
%% dht message(protocol) encode/decode 
%% 06.06.2013
%%
-module(msg).
-compile(export_all).
-export([encode/1,
         ping/2,
         res_ping/2,
         find_node/3,
         res_find_node/3,
         get_peer/3,
         decode_nodes/1,
         decode_peers/1,
         res_get_peer/5,
         announce_peer/5,
         res_announce_peer/2]).
-define(ADAPTER(L), to_dict(L)).
-define(IDSIZE, 20).
-define(ADDRSIZE, 6).
-define(NODESIZE, 26).

encode({ping, Tid, MyID, _Arg}) ->
    ping(Tid, MyID);

encode({find_node, Tid, MyID, {Target}}) ->
    find_node(Tid, MyID, Target);

encode({get_peers, Tid, MyID, {InfoHash}}) ->
    get_peer(Tid, MyID, InfoHash);

encode({announce_peer, Tid, MyID, {InfoHash, Token, Port}}) ->
    announce_peer(Tid, MyID, InfoHash, Token, Port).

%% These functions below are used to adapter to `bencode' module
convert_value(V) ->
    if 
        is_atom(V) -> list_to_binary(atom_to_list(V));
        is_list(V) -> list_to_binary(V);
        true -> V
    end.

convert_pair({K, V}) ->
    {convert_value(K), convert_value(V)}.

to_list(L) ->
    lists:map(fun({K, V}) -> convert_pair({K, V}) end, L).

to_dict(L) ->
    {dict, dict:from_list(to_list(L))}.

query(Tid, Type, Arg) ->
    ?ADAPTER([{t, Tid}, {y, q}, {q, Type}, {a, Arg}]).

response(Tid, Arg) ->
    ?ADAPTER([{t, Tid}, {y, r}, {r, Arg}]).

bjoin(List) ->
    lists:foldr(fun(A, B) -> <<A/binary, B/binary>> end, <<>>, List).

% ownid
ping(Tid, Id) ->
    Arg = ?ADAPTER([{id, Id}]),
    bencode:encode(query(Tid, ping, Arg)).

% ownid
res_ping(Tid, Id) ->
    Arg = ?ADAPTER([{id, Id}]),
    bencode:encode(response(Tid, Arg)).

% Id: ownid
find_node(Tid, Id, Target) ->
    Arg = ?ADAPTER([{id, Id}, {target, Target}]),
    bencode:encode(query(Tid, find_node, Arg)).

% encode_compact_addr({127, 0, 0, 1}, 1024)
encode_compact_addr(IP, Port) when is_tuple(IP) ->
    {A1, A2, A3, A4} = IP,
    <<A1:8, A2:8, A3:8, A4:8, Port:16>>.

decode_compact_addr(B) ->
    <<A1:8, A2:8, A3:8, A4:8, Port:16>> = B,
    {{A1, A2, A3, A4}, Port}.

encode_compact_node({Id, IP, Port}) when is_binary(Id) ->
    bjoin([Id, encode_compact_addr(IP, Port)]);

encode_compact_node({Id, IP, Port}) when is_integer(Id) ->
    BID = <<Id:160>>,
    encode_compact_node({BID, IP, Port}).

% return as: {<<abcd...>>, {127, 0, 0, 1}, 1111}
decode_compact_node(B) ->
    <<Node:?IDSIZE/binary, B1:?ADDRSIZE/binary>> = B,
    {IP, Port} = decode_compact_addr(B1),
    {Node, IP, Port}.

% [{Id, IP, Port}, {Id, IP, Port}]
encode_nodes([Node|Rest]) ->
    bjoin([encode_compact_node(Node), encode_nodes(Rest)]);

encode_nodes([]) ->
    <<>>.

% return as: [{<<acc...>>, {127, 0, 0, 1}, 1111},...
decode_nodes(<<N:?NODESIZE/binary, R/binary>>) ->
    [decode_compact_node(N)]++decode_nodes(R);

decode_nodes(<<>>) ->
    [].

% Id: ownid
res_find_node(Tid, Id, Nodes) ->
    NodesStr = encode_nodes(Nodes),
    Arg = ?ADAPTER([{id, Id}, {nodes, NodesStr}]),
    bencode:encode(response(Tid, Arg)).

get_peer(Tid, Id, Info) ->
    Arg = ?ADAPTER([{id, Id}, {info_hash, Info}]),
    bencode:encode(query(Tid, get_peers, Arg)).

% encode_peers([{{127, 0, 0, 1}, 1111}, {{127, 0, 0, 1}, 1111}]
encode_peers([Peer|Rest]) ->
    {IP, Port} = Peer,
    [encode_compact_addr(IP, Port)]++encode_peers(Rest);

encode_peers([]) ->
    [].

% return as: [{{127, 0, 0, 1}, 111},...
decode_peers(<<P:?ADDRSIZE/binary, R/binary>>) ->
    [decode_compact_addr(P)]++decode_peers(R);

decode_peers(<<>>) ->
    [].

% Id: ownid
res_get_peer(Tid, Id, Token, peers, Peers) ->
    Arg = ?ADAPTER([{id, Id}, {token, Token}, {values, encode_peers(Peers)}]),
    bencode:encode(response(Tid, Arg));

res_get_peer(Tid, Id, Token, nodes, Nodes) ->
    Arg = ?ADAPTER([{id, Id}, {token, Token}, {nodes, encode_nodes(Nodes)}]),
    bencode:encode(response(Tid, Arg)).

announce_peer(Tid, Id, Info, Token, Port) ->
    Arg = ?ADAPTER([{id, Id}, {token, Token}, {info_hash, Info}, {port, Port}]),
    bencode:encode(query(Tid, announce_peer, Arg)).

res_announce_peer(Tid, Id) ->
    Arg = ?ADAPTER([{id, Id}]),
    bencode:encode(response(Tid, Arg)).

