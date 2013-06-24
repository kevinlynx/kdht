%%
%% bucket.erl
%% Kevin Lynx
%% 06.05.2013
%%
-module(bucket).
-export([new/0,
         members/2,
         all/1,
         closest/2,
         closest/3,
         closest_to/3,
         is_member/4,
         delete/4,
         dump/1,
         insert/5]).
-define(in_range(ID, Min, Max), ((ID >= Min) and (ID < Max))).
-include("conf.hrl").

new() ->
    Max = dht_id:max(),
    Min = dht_id:min(),
    [{Min, Max, []}].

% return new Buckets
insert(Self, ID, IP, Port, Buckets) ->
    do_insert(Self, ID, IP, Port, Buckets).

members(ID, [{Min, Max, Members}|_]) when ?in_range(ID, Min, Max) ->
    Members;
    
members(ID, [_|T]) ->
    members(ID, T).

% return all nodes in all buckets
all([]) ->
    [];

all([{_, _, Members}|T]) ->
    Members++all(T).

% find ?K closest nodes to target in all buckets
closest(Target, Buckets) ->
    closest(Target, Buckets, ?K).

closest(Target, Buckets, Num) ->
    Nodes = all(Buckets),
    closest_to(Target, Nodes, Num).

closest_to(Target, Nodes, Num) ->
    WithDist = [{dht_id:distance(Target, ID), {ID, IP, Port}} || 
        {ID, IP, Port} <- Nodes],
    Sorted = lists:sort(WithDist),
    Limited = case length(Sorted) > Num of
        true -> 
            {Prev, _} = lists:split(Num, Sorted),
            Prev;
        false -> 
            Sorted
    end,
    [Node || {_, Node} <- Limited].

% check if a node is a member of a bucket list
is_member(_, _, _, []) ->
    false;
is_member(ID, IP, Port, [{Min, Max, Members}|_])
when ?in_range(ID, Min, Max) ->
    lists:member({ID, IP, Port}, Members);
is_member(ID, IP, Port, [_|T]) ->
    is_member(ID, IP, Port, T).

% delete a Node from bucket list
delete(_, _, _, []) ->
    [];
delete(ID, IP, Port, [{Min, Max, Members}|T])
when ?in_range(ID, Min, Max) ->
    NewMembers = ordsets:del_element({ID, IP, Port}, Members),
    [{Min, Max, NewMembers}|T];
delete(ID, IP, Port, [H|T]) ->
    [H|delete(ID, IP, Port, T)].

dump(Buckets) ->
    {ok, FP} = file:open("buckets.txt", [write]),
    io:format(FP, "~p buckets~n", [length(Buckets)]),
    dump_bucket(FP, Buckets),
    file:close(FP).

dump_bucket(FP, [{Min, Max, Members}|T]) ->
    io:format(FP, "~nBucket [~s, ~s) ~p nodes~n", 
        [dht_id:tohex(Min), dht_id:tohex(Max), length(Members)]),
    [io:format(FP, "~s ~p:~p~n", [dht_id:tohex(ID), IP, Port]) 
        || {ID, IP, Port} <- Members],
    dump_bucket(FP, T);

dump_bucket(_FP, []) ->
    ok. 

do_insert(Self, ID, IP, Port, [{Min, Max, Members}|T]) 
when ?in_range(ID, Min, Max), ?in_range(Self, Min, Max) ->
    NumMembers = length(Members),
    if 
        NumMembers < ?K ->
            % ordsets will keep the element unique
            NewMembers = ordsets:add_element({ID, IP, Port}, Members),
            [{Min, Max, NewMembers}|T];

        NumMembers == ?K, (Max - Min) > 2 ->
            Diff  = Max - Min,
            Half  = Max - (Diff div 2),
            Lower = [N || {MID, _, _}=N <- Members, ?in_range(MID, Min, Half)],
            Upper = [N || {MID, _, _}=N <- Members, ?in_range(MID, Half, Max)],
            WithSplit = [{Min, Half, Lower}, {Half, Max, Upper}|T],
            do_insert(Self, ID, IP, Port, WithSplit);

        NumMembers == ?K ->
           [{Min, Max, Members}|T]
    end;

do_insert(_, ID, IP, Port, [{Min, Max, Members}|T])
when ?in_range(ID, Min, Max) ->
    NumMembers = length(Members),
    if  
        NumMembers < ?K ->
            NewMembers = ordsets:add_element({ID, IP, Port}, Members),
            [{Min, Max, NewMembers}|T];
        NumMembers == ?K ->
            [{Min, Max, Members}|T]
    end;

do_insert(Self, ID, IP, Port, [H|T]) ->
    [H|do_insert(Self, ID, IP, Port, T)].
