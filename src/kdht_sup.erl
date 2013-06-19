%%
%% kdht_sup.erl
%% Kevin Lynx
%%
-module(kdht_sup).
-include("vlog.hrl").
-include("conf.hrl").
-behaviour(supervisor).
-export([init/1]).
-export([start_link/4, 
		 save_state/1,
		 stop/1]).

start_link(StateFile, Port, Mod, OptID) ->
	supervisor:start_link(?MODULE, [StateFile, Port, Mod, OptID]).

stop(Pid) ->
	save_state(Pid),
	exit(Pid, normal).

save_state(Pid) ->
	MyID = get_dhtid(Pid),
	dht_state:save(MyID).

init([StateFile, Port, Mod, OptID]) ->
	random:seed(now()),
	{MyID, NodeAddrs} = case dht_state:load(StateFile) of
		{error, _} -> 
			{OptID, []};
		Ret ->
			Ret
	end,
	save_dhtid(MyID),
	Spec = {one_for_all, 1, 600},
	Children = [{timer_monitor, {timer_monitor, start_link, [MyID]}, permanent, 1000, worker, dynamic},
		{storage, {storage, start_link, [MyID]}, permanent, 1000, worker, dynamic},
		{dht_net, {dht_net, start_link, [MyID, Port]}, permanent, 1000, worker, dynamic},
		{dht_state, {dht_state, start_link, [MyID, NodeAddrs, StateFile, Mod]}, permanent, 1000, worker, dynamic}],
	{ok, {Spec, Children}}.

save_dhtid(ID) ->
	put(dhtid, ID).

get_dhtid(Pid) ->
	PInfo = process_info(Pid),
	Dict = proplists:get_value(dictionary, PInfo),
	proplists:get_value(dhtid, Dict).
