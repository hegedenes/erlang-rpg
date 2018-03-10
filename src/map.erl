-module(map).

-export([start/1, init/1]).

-include("db.hrl").

start(Args) ->
    spawn(map, init, [Args]).

init(Args) ->
    io:format("map:init\n"),
    Map = #map{
            id   = proplists:get_value(id, Args, 0),
            pid  = self(),
            type = proplists:get_value(type, Args, normal)
        },
    base:save_map(Map),
    update(Map, []).

update(Map, Monsters) ->
    receive
        monster ->
            IDs = base:get_monster_ids(),
            ID = lists:nth(util:rand(1, length(IDs)), IDs),
            monster:start([{id, ID}, {map, Map#map.id}]),
            update(Map, Monsters);
        {add, Pid} ->
            update(Map, [Pid | Monsters]);
        {remove, Pid} ->
            update(Map, lists:delete(Pid, Monsters));
        flower ->
            base:replace_object("Flower", world:random_pos()),
            update(Map, Monsters);
        stop ->
            [Pid ! die || Pid <- Monsters]
    after 10 ->
        % io:format("monster count: ~p\n", [length(Monsters)]),
        case length(Monsters) < ?MONSTER_COUNT of
            true  -> self() ! monster;
            false -> ok
        end,
        update(Map, Monsters)
    end.
