%%
%% dht_state.erl
%% Kevin Lynx
%% 06.06.2013
%%
-module(dht_state).
-behaviour(gen_server).
-include("vlog.hrl").
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/3,
		 start_link/4,
		 insert/4,
		 stop/1,
		 load/1,
		 save/1,
		 notify_event/3,
		 dump/1,
		 active_node/4,
		 closest/3]).
% private callbacks
-export([keep_node_alive/4,
		 join/3, 
		 do_startup_insert/2,
		 find_self/1]).
-record(state, {savefile, ownid, mod, buckets, savetimer}).
-include("conf.hrl").
-define(NODE_ID(ID, IP, Port), {node, ID, IP, Port}).

closest(MyID, Target, Num) ->
	gen_server:call(srv_name(MyID), {closest, Target, Num}).

insert(MyID, ID, IP, Port) ->
	gen_server:cast(srv_name(MyID), {insert, ID, IP, Port}).

% mark the active time for a node
active_node(MyID, ID, IP, Port) ->
	Key = ?NODE_ID(ID, IP, Port),
	case timer_monitor:get(MyID, Key) of
		error ->
			?W(?FMT("timer monitor node not exist ~s", [dht_id:tohex(ID)]));
		_ ->
			timer_monitor:active(MyID, Key)
	end.

notify_event(MyID, Event, Args) ->
	gen_server:cast(srv_name(MyID), {event, Event, Args}).

dump(MyID) ->
	gen_server:cast(srv_name(MyID), dump).

start_link(MyID, Filename, Mod) ->
	start_link(MyID, [], Filename, Mod).

start_link(MyID, NodeAddrs, Filename, Mod) ->
	gen_server:start_link({local, srv_name(MyID)}, ?MODULE, [MyID, NodeAddrs, Filename, Mod], []).

stop(MyID) ->
	gen_server:cast(srv_name(MyID), stop).

save(MyID) ->
	gen_server:cast(srv_name(MyID), save).

srv_name(MyID) ->
	list_to_atom("dht_state" ++ dht_id:tohex(MyID)).

init([MyID, NodeAddrs, Filename, Mod]) ->
	Buckets = bucket:new(),
	startup_insert_nodes(MyID, NodeAddrs),
	{ok, TRef} = timer:send_interval(?SAVE_INTERVAL, {interval_save}),
	{ok, #state{ownid = MyID, savefile = Filename, 
		savetimer = TRef, mod = Mod, buckets = Buckets}}.

handle_cast(save, State) ->
	save_state(State),
	{noreply, State};

handle_cast(dump, State) ->
	#state{buckets = Buckets} = State,
	bucket:dump(Buckets),
	{noreply, State};

handle_cast({event, Event, Args}, State) ->
	#state{mod = M} = State,
	M:handle_event(Event, Args),
	{noreply, State};

handle_cast({insert, ID, IP, Port}, State) ->
	#state{ownid = MyID, buckets = Buckets} = State,
	Exist = bucket:is_member(ID, IP, Port, Buckets),
	NewBuckets = bucket:insert(MyID, ID, IP, Port, Buckets),
	monitor_node(MyID, ID, IP, Port, Exist),
	?T(?FMT("inserted a node, bucket size ~p", [length(NewBuckets)])),
	case length(NewBuckets) > 160 of
		true -> ?E(?FMT("fatal error, bucket size ~p > 160", [length(NewBuckets)]));
		false -> ok
	end,
	{noreply, State#state{buckets = NewBuckets}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_, State) ->
	#state{savetimer = TRef} = State,
	timer:cancel(TRef),
	save_state(State),
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_call({get_ownid}, _From, State) ->
	#state{ownid = MyID} = State,	
	{reply, MyID, State};

handle_call({closest, Target, Num}, _From, State) ->
	#state{buckets = Buckets} = State,
	Closest = bucket:closest(Target, Buckets, Num),
	{reply, Closest, State};

handle_call({delete, ID, IP, Port}, _From, State) ->
	#state{ownid = MyID, buckets = Buckets} = State,
	NewBuckets = bucket:delete(ID, IP, Port, Buckets),
	stop_monitor_node(MyID, ID, IP, Port),
	?T(?FMT("delete a node ~s", [dht_id:tohex(ID)])),
	{reply, ok, State#state{buckets = NewBuckets}};

handle_call(_, _From, State) ->
	{reply, not_implemented, State}.

handle_info({interval_save}, State) ->
	save_state(State),
	{noreply, State};

handle_info({node_timeout, ID, IP, Port}, State) ->
	#state{ownid = MyID} = State,
	?I(?FMT("node timeout ~s", [dht_id:tohex(ID)])),
	spawn_link(?MODULE, keep_node_alive, [MyID, ID, IP, Port]),
	{noreply, State};

handle_info(_, State) ->
    {noreply, State}.

% TODO: monitor bucket
monitor_node(MyID, ID, IP, Port, false) ->
	Msg = {node_timeout, ID, IP, Port},
	Key = ?NODE_ID(ID, IP, Port),
	?T(?FMT("timer monitor a new node ~s", [dht_id:tohex(ID)])),
	case timer_monitor:get(MyID, Key) of
		error ->
			timer_monitor:add(MyID, Key, ?NODE_TIMEOUT, Msg);
		_ ->
			?W(?FMT("timer monitor on node ~s exist", [dht_id:tohex(ID)]))
	end;

monitor_node(MyID, ID, IP, Port, true) ->
	Key = ?NODE_ID(ID, IP, Port),
	?T(?FMT("active a node ~s", [dht_id:tohex(ID)])),
	timer_monitor:active(MyID, Key).

stop_monitor_node(MyID, ID, IP, Port) ->
	Key = ?NODE_ID(ID, IP, Port),
	?T(?FMT("stop a node ~s timer monitor", [dht_id:tohex(ID)])),
	timer_monitor:remove(MyID, Key).

keep_node_alive(MyID, ID, IP, Port) ->
	ping_node(MyID, ID, IP, Port, 3).

ping_node(MyID, ID, IP, Port, Count) when Count > 0 ->
	% dht_net:ping will update the node active time if the query get response
	case dht_net:ping(MyID, IP, Port)	of
		timeout ->
			ping_node(MyID, ID, IP, Port, Count - 1);
		_ -> ok
	end;

ping_node(MyID, ID, IP, Port, 0) ->
	?I(?FMT("node keepalive failed, inactive the node ~s", [dht_id:tohex(ID)])),
	% TODO: keep the node in the bucket until another node arrives
	% If we delete the node, the bucket will be empty
	gen_server:call(srv_name(MyID), {delete, ID, IP, Port}),
	ok.

save_state(State) ->
	#state{ownid = MyID, savefile = Filename, buckets = Buckets} = State,
	WithID = bucket:all(Buckets),
	Addrs = [{IP, Port} || {_ID, IP, Port} <- WithID],
	save(Filename, MyID, Addrs).

save(Filename, Self, NodeList) ->
	PersistentState = [{node_id, Self}, {node_set, NodeList}],
    file:write_file(Filename, term_to_binary(PersistentState)).

load(Filename) ->
    case file:read_file(Filename) of
        {ok, BinState} ->
            PersistentState = binary_to_term(BinState),
            {value, {_, Self}}  = lists:keysearch(node_id, 1, PersistentState),
            {value, {_, Nodes}} = lists:keysearch(node_set, 1, PersistentState),
            ?I(?FMT("load state from ~s success", [Filename])),
            ?I(?FMT("self: ~s, nodes count: ~p", [dht_id:tohex(Self), length(Nodes)])),
            {Self, Nodes};
        {error, Reason} ->
        	?I(?FMT("load state from ~s failed ~p", [Filename, Reason])),
        	{error, Reason}
    end.

startup_insert_nodes(MyID, []) ->
	{ok, IP} = inet:getaddr("dht.transmissionbt.com", inet),
	Port = 6881,
	?I(?FMT("initial startup, try to join 1st node ~p:~p", [IP, Port])),
	spawn_link(?MODULE, join, [MyID, IP, Port]);

startup_insert_nodes(MyID, Addrs) ->
	spawn_link(?MODULE, do_startup_insert, [MyID, Addrs]).

do_startup_insert(MyID, Addrs) ->
	PingRets = [dht_net:ping(MyID, IP, Port) || {IP, Port} <- Addrs],
	case [Ret || Ret <- PingRets, Ret =/= timeout] of
		[] ->
			?W(?FMT("not even insert one node ~p", [length(Addrs)])),
			startup_insert_nodes(MyID, []);
		_ ->
			notify_event(MyID, startup, {MyID})
	end.

join(MyID, IP, Port) ->
	case dht_net:ping(MyID, IP, Port) of
		timeout ->
			join(MyID, IP, Port);
		_ ->
			?I(?FMT("join to ~p:~p success", [IP, Port])),
			spawn_link(?MODULE, find_self, [MyID])
	end.

find_self(MyID) ->
	search:find_node(MyID, MyID),
	notify_event(MyID, startup, {MyID}).


