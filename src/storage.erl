%%
%% storage.erl
%% Kevin Lynx
%% 06.09.2013
%%
-module(storage).
-behaviour(gen_server).
-include("vlog.hrl").
-export([init/1, 
		 handle_info/2, 
		 handle_cast/2, 
		 handle_call/3, 
		 code_change/3, 
		 terminate/2]).
-export([start_link/1,
		 stop/1,
		 query/2,
		 insert/4]).
-record(state, {hashes}).
-include("conf.hrl").

start_link(MyID) ->
	gen_server:start_link({local, srv_name(MyID)}, ?MODULE, [], []).

stop(MyID) ->
	gen_server:cast(srv_name(MyID), stop).

query(MyID, InfoHash) ->
	gen_server:call(srv_name(MyID), {query, InfoHash}).

insert(MyID, InfoHash, IP, Port) ->
	gen_server:call(srv_name(MyID), {insert, InfoHash, IP, Port}).

srv_name(MyID) ->
	list_to_atom("dht_storage" ++ dht_id:tohex(MyID)).

init([]) ->
	{ok, #state{hashes = gb_trees:empty()}}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_call({query, InfoHash}, _From, State) ->
	#state{hashes = Hashes} = State,
	Peers = case gb_trees:is_defined(InfoHash, Hashes) of
		false -> 
			[];
		true -> 
			{_, Exist} = gb_trees:get(InfoHash, Hashes),
			Exist
	end,
	OnlyAddr = [{IP, Port} || {IP, Port, _} <- Peers],
	{reply, OnlyAddr, State};

handle_call({insert, InfoHash, IP, Port}, _From, #state{hashes = Hashes} = State) ->
	{T, Exist} = case gb_trees:is_defined(InfoHash, Hashes) of
		false -> 
			{ok, TRef} = timer:send_interval(?CHECK_INTERVAL, {expire, InfoHash}),
			?T(?FMT("start a new hash timer ~p", [TRef])),
			{TRef, []};
		true -> 
			gb_trees:get(InfoHash, Hashes)
	end,
	?I(?FMT("insert a peer ~p:~p for hash ~s", [IP, Port, dht_id:tohex(InfoHash)])),
	New = Exist ++ [{IP, Port, now()}],
	NewTree = gb_trees:enter(InfoHash, {T, New}, Hashes),
	{reply, ok, State#state{hashes = NewTree}}.

handle_info({expire, InfoHash}, State) ->
	#state{hashes = Hashes} = State,
	{TRef, Peers} = gb_trees:get(InfoHash, Hashes),
	NewPeers = [{IP, Port, Time} || {IP, Port, Time} <- Peers, 
		(timer:now_diff(now(), Time) div 1000) < ?EXPIRETIME],
	NewHashes = case length(NewPeers) == 0 of
		true -> 
			timer:cancel(TRef),
			?T(?FMT("expire ~p peers (all) for infohash ~s", [length(Peers), dht_id:tohex(InfoHash)])),
			gb_trees:delete(InfoHash, Hashes);
		false ->
			?T(?FMT("expire ~p peers for infohash ~s", 
				[length(Peers) - length(NewPeers), dht_id:tohex(InfoHash)])),
			gb_trees:update(InfoHash, {TRef, NewPeers}, Hashes)
	end,
	{noreply, State#state{hashes = NewHashes}};

handle_info(_, State) ->
    {noreply, State}.

