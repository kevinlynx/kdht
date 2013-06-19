%%
%% search.erl
%% copied/modified from etorrent dht implementation
%% Kevin Lynx
%% 06.07.2013
%%
-module(search).
-include("vlog.hrl").
-export([find_node/2,
		 get_peers/2]).

find_node(MyID, NodeID) ->
    Width = search_width(),
    Retry = search_retries(),
    dht_iter_search(MyID, find_node, NodeID, Width, Retry).

% {Trackers, Peers, AliveNodes}
% `Trackers' are nodes which track the infohash
% `Peers' are peers which are downloading the infohash
get_peers(MyID, InfoHash) ->
    Width = search_width(),
    Retry = search_retries(),
    dht_iter_search(MyID, get_peers, InfoHash, Width, Retry).

search_width() ->
    32.

search_retries() ->
    4.

dht_iter_search(MyID, SearchType, Target, Width, Retry)  ->
	?T(?FMT("start a ~p search to ~s", [SearchType, dht_id:tohex(Target)])),
	% query these nodes in the next iteration
    Next = dht_state:closest(MyID, Target, Width),
    WithDist = [{dht_id:distance(ID, Target), ID, IP, Port} || {ID, IP, Port} <- Next],
    % nodes which have been queried already
    Queried = gb_sets:empty(),
    % responsed nodes are alive
    Alive = gb_sets:empty(),
    dht_iter_search(MyID, SearchType, Target, Width, Retry, 0, WithDist, Queried, Alive, []).

% reach the max retries, stop the search
dht_iter_search(_MyID, SearchType, _, _, Retry, Retry, _, _, Alive, WithPeers) ->
    TmpAlive  = gb_sets:to_list(Alive),
    AliveList = [{ID, IP, Port} || {_, ID, IP, Port} <- TmpAlive],
    case SearchType of
        find_node ->
            AliveList;
        get_peers ->
            Trackers = [{ID, IP, Port, Token} || {ID, IP, Port, Token, _} <- WithPeers],
            Peers = [Peers || {_, _, _, _, Peers} <- WithPeers],
            {Trackers, Peers, AliveList}
    end;

% `WithPeers' are received peers in this search
dht_iter_search(MyID, SearchType, Target, Width, Retry, Retries,
                Next, Queried, Alive, WithPeers) ->
	?T(?FMT("ready to query ~p nodes in this query", [length(Next)])),
    % the next nodes will be queried in this iteration, mark them
    AddQueried = [{ID, IP, Port} || {_, ID, IP, Port} <- Next],
    % the all queried nodes in this search
    NewQueried = gb_sets:union(Queried, gb_sets:from_list(AddQueried)),
    % Query all nodes in the queue and generate a list of
    % {Dist, ID, IP, Port, Nodes} elements
    SearchCalls = [case SearchType of
        find_node -> {dht_net, find_node, [MyID, IP, Port, Target]};
        get_peers -> {dht_net, get_peers, [MyID, IP, Port, Target]}
    end || {_, _, IP, Port} <- Next],
    % call dht_net functions, send queries to remote node and
    % wait here until the node responsed
    ReturnValues = rpc:parallel_eval(SearchCalls),
    WithArgs = lists:zip(Next, ReturnValues),

    FailedCall = make_ref(),
    % filter successful query, a query got responsed
    TmpSuccessful = [case {repack, SearchType, RetVal} of
        {repack, _, {error, timeout}} ->
            FailedCall;
        {repack, find_node, {NID, Nodes}} ->
            {{Dist, NID, IP, Port}, Nodes};
        {repack, get_peers, {NID, Token, Peers, Nodes}} ->
            {{Dist, NID, IP, Port}, {Token, Peers, Nodes}};
        {repack, _, Error} ->
            ?W(?FMT("search error ~p", [Error])),
            FailedCall % process terminate etc...
    end || {{Dist, _ID, IP, Port}, RetVal} <- WithArgs],
    % and now `Successful' contains valid values from `find_node' or `get_peers'
    Successful = [E || E <- TmpSuccessful, E =/= FailedCall],
    % mark all nodes that responded as alive
    AddAlive = [N || {{_, _, _, _}=N, _} <- Successful],
    % all alive nodes in this search
    NewAlive = gb_sets:union(Alive, gb_sets:from_list(AddAlive)),

    % Accumulate all nodes from the successful responses.
    % Calculate the relative distance to all of these nodes
    % and keep the closest nodes which has not already been
    % queried in a previous iteration
    NodeLists = [case {acc_nodes, {SearchType, Res}} of
        {acc_nodes, {find_node, Nodes}} ->
            Nodes;
        {acc_nodes, {get_peers, {_, _, Nodes}}} ->
            Nodes
    end || {_, Res} <- Successful],
    AllNodes  = lists:flatten(NodeLists),
    NewNodes  = [Node || Node <- AllNodes, not gb_sets:is_member(Node, NewQueried)],
    % the next queried nodes are the closest nodes in the responsed nodes replied 
    % from node queried in this search iteration.
    NewNext   = [{dht_id:distance(ID, Target), ID, IP, Port}
                ||{ID, IP, Port} <- bucket:closest_to(Target, NewNodes, Width)],

    % Check if the closest node in the work queue is closer
    % to the target than the closest responsive node that was
    % found in this iteration.
    MinAliveDist = case gb_sets:size(NewAlive) of
        0 ->
            infinity;
        _ ->
            {IMinAliveDist, _, _, _} = gb_sets:smallest(NewAlive),
            IMinAliveDist
    end,
    MinQueueDist = case NewNext of
        [] ->
            infinity;
        Other ->
            {MinDist, _, _, _} = lists:min(Other),
            MinDist
    end,
    % Check if the closest node in the work queue is closer
    % to the infohash than the closest responsive node.
    NewRetries = if
        (MinQueueDist <  MinAliveDist) -> 0;
        (MinQueueDist >= MinAliveDist) -> Retries + 1
    end,
    % Accumulate the trackers and peers found if this is a get_peers search.
    NewWithPeers = case SearchType of
        find_node -> [] = WithPeers;
        get_peers ->
            Tmp = [{ID, IP, Port, Token, Peers}
                || {{_, ID, IP, Port}, {Token, Peers, _}} <- Successful, Peers > []],
            ?T(?FMT("got ~p peers in this iteation", [length(Tmp)])),
            WithPeers ++ Tmp
    end,
    dht_iter_search(MyID, SearchType, Target, Width, Retry, NewRetries,
                    NewNext, NewQueried, NewAlive, NewWithPeers).
