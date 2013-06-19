## kdht

kdht is an erlang [DHT](http://en.wikipedia.org/wiki/Distributed_hash_table) implementation. You can use this library to join a DHT network, and search torrent info-hash in the network.

## Usage

* Download this library source code
* Compile it by `erl` in the library root directory:

        erl -make

* Start `erl` and specify the module beam path:

        erl -pa ebin

* Run test

        testdht:start().

## Example

    -module(testdht).
    -include("vlog.hrl").
    -export([start/0, stop/1, handle_event/2]).
    -export([tell_more_nodes/1]).

    start() ->
        vlog:start_link("kdht.txt", ?TRACE),
        random:seed(now()),
        kdht_sup:start_link("dhtstate.dat", 6882, ?MODULE, dht_id:random()).

    stop(Pid) ->
        kdht_sup:stop(Pid).

    handle_event(announce_peer, {InfoHash, _IP, _BTPort}) ->
        MagHash = dht_id:tohex(InfoHash),
        {ok, FP} = file:open("magnet-link.txt", [append]),
        io:format(FP, "~s~n", [MagHash]),
        file:close(FP);

    handle_event(startup, {MyID}) ->
        spawn_link(?MODULE, tell_more_nodes, [MyID]).

    tell_more_nodes(MyID) ->
        [search:get_peers(MyID, dht_id:random()) || _ <- lists:seq(1, 10)],
        ?I("tell more nodes done").

## TODO

* token implementation


