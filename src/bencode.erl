-module(bencode).

%% API
-export([decode/1, encode/1]).
-export([dec/1]).

%% You are able to choose the dict implementation
-define(DICT, dict).
%%-define(DICT, orddict).

%%====================================================================
%% API
%%====================================================================
decode(Data) ->
    case catch dec(Data) of
	{'EXIT', _} ->
	    {error, unparsed};
	{Res, _} ->
	    {ok, Res}
    end.

encode(Struct) ->
    iolist_to_binary(enc(Struct)).

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Decoding
%%--------------------------------------------------------------------
dec(<<$l, Tail/binary>>) ->
    dec_list(Tail, []);
dec(<<$d, Tail/binary>>) ->
    dec_dict(Tail, ?DICT:new());
dec(<<$i, Tail/binary>>) ->
    dec_int(Tail, []);
dec(Data) ->
    dec_string(Data, []).

dec_int(<<$e, Tail/binary>>, Acc) ->
    {list_to_integer(lists:reverse(Acc)), Tail};
dec_int(<<X, Tail/binary>>, Acc) ->
    dec_int(Tail, [X|Acc]).

dec_string(<<$:, Tail/binary>>, Acc) ->
    Int = list_to_integer(lists:reverse(Acc)),
    <<Str:Int/binary, Rest/binary>> = Tail,
    {Str, Rest};
dec_string(<<X, Tail/binary>>, Acc) ->
    dec_string(Tail, [X|Acc]).

dec_list(<<$e, Tail/binary>>, Acc) ->
    {{list, lists:reverse(Acc)}, Tail};
dec_list(Data, Acc) ->
    {Res, Tail} = dec(Data),
    dec_list(Tail, [Res|Acc]).

dec_dict(<<$e, Tail/binary>>, Acc) ->
    {{dict, Acc}, Tail};
dec_dict(Data, Acc) ->
    {Key, Tail1} = dec(Data),
    {Val, Tail2} = dec(Tail1),
    dec_dict(Tail2, ?DICT:store(Key, Val, Acc)).

%%--------------------------------------------------------------------
%% Encoding
%%--------------------------------------------------------------------
enc(Int) when is_integer(Int) ->
    IntBin = list_to_binary(integer_to_list(Int)),
    [$i, IntBin, $e];
enc(Str) when is_list(Str) ->
    enc(list_to_binary(Str));
enc(Str) when is_binary(Str) ->
    IntBin = list_to_binary(integer_to_list(size(Str))),
    [IntBin, $:, Str];
enc({list, List}) when is_list(List) ->
    [$l, [enc(Elem) || Elem <- List], $e];
enc({dict, Dict}) ->
    Data = lists:map(
	     fun({Key, Val}) when is_list(Key) or is_binary(Key) ->
		     [enc(Key), enc(Val)]
	     end, lists:keysort(1, ?DICT:to_list(Dict))),
    [$d, Data, $e].

