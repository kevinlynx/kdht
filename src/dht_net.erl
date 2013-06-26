%%
%% dht_net.erl
%% Kevin Lynx
%% 06.06.2013
%%
-module(dht_net).
-behaviour(gen_server).
-export([init/1, 
		 handle_info/2, 
		 handle_cast/2, 
		 handle_call/3, 
		 code_change/3, 
		 terminate/2]).
-export([start_link/2,
		 ping/3,
		 find_node/4,
		 get_peers/4,
		 announce_peer/5,
	     stop/1]).
-include("vlog.hrl").
-include("conf.hrl").
-record(state, {ownid, sock, sents, tidseed = 0}).

start_link(MyID, Port) ->
	gen_server:start_link({local, srv_name(MyID)}, ?MODULE, [MyID, Port], []).

stop(MyID) ->
	gen_server:cast(srv_name(MyID), stop).

ping(MyID, IP, Port) ->
	Query = {query, ping, IP, Port, {}},
	case gen_server:call(srv_name(MyID), Query) of
		timeout -> timeout;
		Values ->
			ID = decode_response(ping, Values),
			dht_state:active_node(MyID, ID, IP, Port),
			ID
	end.

find_node(MyID, IP, Port, Target) ->
	Query = {query, find_node, IP, Port, {dht_id:list_id(Target)}},
	case gen_server:call(srv_name(MyID), Query) of
		timeout -> {error, timeout};
		Values ->
			{ResID, Nodes} = decode_response(find_node, Values),
			dht_state:active_node(MyID, ResID, IP, Port),
			{ResID, Nodes}
	end.

get_peers(MyID, IP, Port, Target) ->
	Query = {query, get_peers, IP, Port, {dht_id:list_id(Target)}},
	case gen_server:call(srv_name(MyID), Query) of
		timeout -> {error, timeout};
		Values ->
			{ResID, Token, Peers, Nodes} = decode_response(get_peers, Values),
			dht_state:active_node(MyID, ResID, IP, Port),
			{ResID, Token, Peers, Nodes}
	end.

announce_peer(MyID, IP, Port, InfoHash, BTPort) ->
	Query = {query, announce, IP, Port, {dht_id:list_id(InfoHash), BTPort}},
	case gen_server:call(srv_name(MyID), Query) of
		timeout -> {error, timeout};
		Values ->
			ResID = decode_response(announce_peer, Values),
			dht_state:active_node(MyID, ResID, IP, Port),
			ResID
	end.

srv_name(MyID) ->
	list_to_atom("dht_net" ++ dht_id:tohex(MyID)).
	
decode_response(ping, Values) ->
	{ok, ID} = dict:find(<<"id">>, Values),
	dht_id:integer_id(ID);

decode_response(find_node, Values) ->
	{ok, ResID} = dict:find(<<"id">>, Values),
	Nodes = case dict:find(<<"nodes">>, Values) of
		{ok, NodesBuf} ->
			decode_compact_nodes(NodesBuf);
		_ -> []
	end,
	?T(?FMT("~s response find_node ~p nodes", [dht_id:tohex(ResID), length(Nodes)])),
	{dht_id:integer_id(ResID), Nodes};

decode_response(get_peers, Values) ->
	{ok, ResID} = dict:find(<<"id">>, Values),
	IntID = dht_id:integer_id(ResID),
	Token = case dict:find(<<"token">>, Values) of
		{ok, T} -> T;
		_ -> 
			?W(?FMT("get_peers response not found token ~s", [dht_id:tohex(ResID)])),
			<<0:32>>
	end,
	case dict:find(<<"values">>, Values) of
		{ok, {list, [PeersBuf]}} ->
			Peers = msg:decode_peers(PeersBuf),
			?T(?FMT("~s response values with ~p peers", [dht_id:tohex(IntID), length(Peers)])),
			{IntID, Token, Peers, []};
		_ ->
			Nodes = decode_compact_nodes(Values),
			?T(?FMT("~s response ~p nodes", [dht_id:tohex(IntID), length(Nodes)])),
			{IntID, Token, [], Nodes}
	end;

decode_response(announce_peer, Values) ->
	{ok, ResID} = dict:find(<<"id">>, Values),
	dht_id:integer_id(ResID).

decode_compact_nodes(Values) ->
	case dict:find(<<"nodes">>, Values) of
		{ok, NodesBuf} ->
			Nodes = msg:decode_nodes(NodesBuf),
			[{dht_id:integer_id(ID), IP, Port} || {ID, IP, Port} <- Nodes];
		_ -> []
	end.

init([MyID, Port]) ->
	?I(?FMT("dht_net start at port ~p", [Port])),
    {ok, Sock} = gen_udp:open(Port, [binary, {active, once}]),
    {ok, #state{ownid = MyID, sock = Sock, sents = gb_trees:empty()}}.

handle_info({udp, Socket, IP, Port, RawData}, State) ->
    NewState = handle_msg(Socket, IP, Port, RawData, State),
    inet:setopts(Socket, [{active,once}]),
    {noreply, NewState};

handle_info({timeout, _, TidNum}, State) ->
	#state{sents = Sents} = State,
	NewSents = response_timeout(TidNum, Sents),
	{noreply, State#state{sents = NewSents}};

handle_info(_Msg, _S) ->
    {noreply, _S}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_call({query, Type, IP, Port, Arg}, From, State) ->
	#state{ownid = MyID, tidseed = TidNum, sock = Sock, sents = Sents} = State,
	Tid = make_tid(Type, TidNum),
	Msg = msg:encode({Type, Tid, dht_id:list_id(MyID), Arg}),
	case gen_udp:send(Sock, IP, Port, Msg) of
		ok ->
			?T(?FMT("send query ~p to ~p:~p", [Type, IP, Port])),
			NewSents = add_query_process(TidNum, From, Sents),
			NewState = State#state{tidseed = TidNum + 1, sents = NewSents},
			{noreply, NewState};
		{error, Reason} ->
			?E(?FMT("udp send error: ~p", [Reason])),
			{reply, timeout, State}
	end;

handle_call(_, _From, State) ->
	{reply, not_implemented, State}.

handle_msg(Socket, IP, Port, Data, State) ->
	#state{ownid = MyID, sents = Sents} = State,
	case (catch parse_message(Data)) of
		{'EXIT', _} ->
			?W(?FMT("parse message failed ~p:~p ~p", [IP, Port, Data])),
			State;
		{error, Tid, Err} ->
			?W(?FMT("received an error ~p ~p from ~p:~p", [Tid, Err, IP, Port])),
			NewSents = response_timeout(Tid, Sents),
			State#state{sents = NewSents};
		{response, Tid, Args} ->
			?T(?FMT("received a response ~p", [Tid])),
			new_node(MyID, response, Args, IP, Port),
			NewSents = response_ok(Tid, Args, Sents),
			State#state{sents = NewSents};
		{Query, Tid, Args} ->
			?T(?FMT("received a query ~p from ~p:~p", [Query, IP, Port])),
			new_node(MyID, query, Args, IP, Port),
			MyListID = dht_id:list_id(MyID),
			case handle_query(Query, MyListID, Tid, Args, IP, Port) of
				{ok, Msg} ->
					gen_udp:send(Socket, IP, Port, Msg);
				F ->
					?W(?FMT("failed with response ~p", [F]))
			end,
			State
	end.

handle_query(ping, MyID, Tid, _Args, _IP, _Port) ->
	{ok, msg:res_ping(Tid, MyID)};

handle_query(find_node, MyID, Tid, Args, _IP, _Port) ->	
	{ok, Target} = dict:find(<<"id">>, Args),
	?T(?FMT("recv find_node ~s", [dht_id:tohex(Target)])),
	TID = dht_id:integer_id(Target),
	Closest = dht_state:closest(MyID, TID, 8),
	Msg = msg:res_find_node(Tid, MyID, Closest),
	{ok, Msg};

handle_query(get_peers, MyID, Tid, Args, IP, Port) ->
    {ok, InfoHash} = dict:find(<<"info_hash">>, Args),
    ?T(?FMT("recv get_peers ~s", [dht_id:tohex(InfoHash)])),
    Token = token_value(IP, Port),
	TID = dht_id:integer_id(InfoHash),
    case storage:query(MyID, TID) of
    	[] ->
			Closest = dht_state:closest(MyID, TID, 8),
			?T(?FMT("response get_peers with ~p nodes", [length(Closest)])),
			{ok, msg:res_get_peer(Tid, MyID, Token, nodes, Closest)};
		Peers ->
			?T(?FMT("response get_peers with ~p peers", [length(Peers)])),
			{ok, msg:res_get_peer(Tid, MyID, Token, peers, Peers)}
	end;

handle_query(announce_peer, MyID, Tid, Args, IP, Port) ->
	{ok, InfoHash} = dict:find(<<"info_hash">>, Args),
	{ok, BTPort} = dict:find(<<"port">>, Args),
	{ok, Token} = dict:find(<<"token">>, Args),	
	?T(?FMT("recv announce_peer ~s, ~p", [dht_id:tohex(InfoHash), BTPort])),
    case token_match(IP, Port, Token) of
        false -> 
            % TODO: response a token error
            ?W("token not match"),
            {error, token_not_match};
        true ->
        	dht_state:notify_event(MyID, announce_peer, {InfoHash, IP, BTPort}),
        	storage:insert(MyID, dht_id:integer_id(InfoHash), IP, BTPort),
            {ok, msg:res_announce_peer(Tid, MyID)}
    end.

new_node(MyID, Reason, ArgDict, IP, Port) ->
	{ok, ID} = dict:find(<<"id">>, ArgDict),
	?T(?FMT("insert a node ~s ~p:~p by ~p", [dht_id:tohex(ID), IP, Port, Reason])),
	dht_state:insert(MyID, dht_id:integer_id(ID), IP, Port).

parse_message(Data) ->
	{ok, {dict, Dict}} = bencode:decode(Data),
	{ok, Tid} = dict:find(<<"t">>, Dict),
	case dict:find(<<"y">>, Dict) of
		{ok, <<"q">>} ->
			{ok, QS} = dict:find(<<"q">>, Dict),
			Type = query_type(QS),
			{ok, {dict, Args}} = dict:find(<<"a">>, Dict),
			assert_query_args(Type, Args),
			assert_args(Args),
			{Type, Tid, Args};
		{ok, <<"r">>} ->
			{ok, {dict, Args}} = dict:find(<<"r">>, Dict),
			assert_args(Args),
			{response, Tid, Args};
		{ok, <<"e">>} ->
			{ok, {list, Err}} = dict:find(<<"e">>, Dict),
			{error, Tid, Err}
	end.

assert_query_args(get_peers, Args) ->
    {ok, InfoHash} = dict:find(<<"info_hash">>, Args),
	20 = byte_size(InfoHash);

assert_query_args(_Type, _Args) ->
	ok.

assert_args(Args) ->
	{ok, ID} = dict:find(<<"id">>, Args),
	20 = byte_size(ID).

% return new sents
response_ok(<<_, _, TidNum:16>> = _, Values, Sents) ->
	case find_query_process(TidNum, Sents) of
		undefined ->
			?W(?FMT("response to a non-exist query ~p", [TidNum])),
			Sents;
		From ->
			gen_server:reply(From, Values),
			remove_query_process(TidNum, Sents)
	end;

% an invalid remote node will reply invalid tid, and the timer
% will remove the query
response_ok(Tid, _Values, Sents) ->
	?W(?FMT("invalid tid in response ~p", [Tid])),
	Sents.

response_timeout(<<_, _, TidNum:16>> = _, Sents) ->
	response_timeout(TidNum, Sents);

response_timeout(TidNum, Sents) ->
	case find_query_process(TidNum, Sents) of
		undefined -> Sents;
		From ->
			gen_server:reply(From, timeout),
			?T(?FMT("query ~p timeout", [TidNum])),
			remove_query_process(TidNum, Sents)
	end.

add_query_process(TidNum, From, Sents) ->
	?T(?FMT("add query ~p", [TidNum])),
	Msg = {timeout, self(), TidNum},
	TRef = erlang:send_after(?QUERY_TIMEOUT, self(), Msg),
	gb_trees:insert(TidNum, {From, TRef}, Sents).	

find_query_process(TidNum, Sents) ->
	case gb_trees:is_defined(TidNum, Sents) of
		true ->
			{From, _} = gb_trees:get(TidNum, Sents),
			From;
		false ->
			undefined
	end.	

remove_query_process(TidNum, Sents) ->
	?T(?FMT("remove query ~p", [TidNum])),
	{_, TRef} = gb_trees:get(TidNum, Sents),
	erlang:cancel_timer(TRef),
	gb_trees:delete(TidNum, Sents).

make_tid(Type, TidNum) ->
    D = case Type of
        ping -> <<"pn">>;
        find_node -> <<"fn">>;
        get_peers -> <<"gp">>;
        announce_peer -> <<"an">>
    end,
    <<D/binary, TidNum:16>>.

query_type(<<"ping">>) -> ping;
query_type(<<"find_node">>) -> find_node;
query_type(<<"get_peers">>) -> get_peers;
query_type(<<"announce_peer">>) -> announce_peer.

%% TODO: implement token
token_value(_IP, _Port) ->
    list_to_binary([0 || _ <- lists:seq(1, 32)]).

token_match(_IP, _Port, _Token) ->
    true.
