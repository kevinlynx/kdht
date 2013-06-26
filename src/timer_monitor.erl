%%
%% timer_monitor.erl
%% Kevin Lynx
%% 06.05.2013
%%
-module(timer_monitor).
-include("vlog.hrl").
-include("conf.hrl").
-behaviour(gen_server).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([start_link/1, 
         stop/1,
         add/4, 
         get/2,
         active/2,
         remove/2]).
-record(state, {timers}).

start_link(MyID) ->
    gen_server:start_link({local, srv_name(MyID)}, ?MODULE, [], []).

stop(MyID) ->
    gen_server:cast(srv_name(MyID), stop).

add(MyID, Key, Timeout, Msg) ->
    gen_server:call(srv_name(MyID), {add, Key, Timeout, Msg}).

get(MyID, Key) ->
    gen_server:call(srv_name(MyID), {get, Key}).

remove(MyID, Key) ->
    gen_server:call(srv_name(MyID), {remove, Key}).

active(MyID, Key) ->
    gen_server:call(srv_name(MyID), {active, Key}).

srv_name(MyID) ->
    list_to_atom("timer_monitor" ++ dht_id:tohex(MyID)).

init([]) ->
    {ok, #state{timers = gb_trees:empty()}}.

handle_info({check, Key, Timeout, From, Msg}, State) ->
    #state{timers = Timers} = State,
    {Active, _} = gb_trees:get(Key, Timers),
    case (timer:now_diff(now(), Active) div 1000) >= Timeout of
        true ->
            From ! Msg;
        false -> continue
    end,
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_, State) ->
    {noreply, State}.

terminate(_, State) ->
    {ok, State}.

code_change(_, _, State) ->
    {ok, State}.

handle_call({add, Key, Timeout, Msg}, From, State) ->
    #state{timers = Timers} = State,
    {Pid, _} = From,
    ThisMsg = {check, Key, Timeout, Pid, Msg},
    {ok, NewRef} = timer:send_interval(Timeout, ThisMsg),
    NewTimers = gb_trees:insert(Key, {now(), NewRef}, Timers),
    check_stats(gb_trees:size(NewTimers)),
    {reply, ok, State#state{timers = NewTimers}};

handle_call({active, Key}, _From, State) ->
    #state{timers = Timers} = State,
    false = gb_trees:is_empty(Timers),
    {_, TRef} = gb_trees:get(Key, Timers),
    NewTimers = gb_trees:update(Key, {now(), TRef}, Timers),
    {reply, ok, State#state{timers = NewTimers}};

handle_call({remove, Key}, _From, State) ->
    #state{timers = Timers} = State,
    {_, TRef} = gb_trees:get(Key, Timers),
    timer:cancel(TRef),
    NewTimers = gb_trees:delete(Key, Timers),
    {reply, ok, State#state{timers = NewTimers}};

handle_call({get, Key}, _From, State) ->
    #state{timers = Timers} = State,
    Active = case gb_trees:is_defined(Key, Timers) of
        true ->
            {A, _} = gb_trees:get(Key, Timers),
            A;
        false ->
            error
    end,
    {reply, Active, State};

handle_call(_, _From, State) ->
    {reply, not_implemented, State}.

check_stats(TimerSize) ->
    case TimerSize > ?MAX_NODE_TIMER of
        true -> ?E(?FMT("~p node timer ~p > ~p", [self(), TimerSize, ?MAX_NODE_TIMER]));
        false -> ok
    end.

