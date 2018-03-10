-module(db_loader).

-export([load/0]).

-include("db.hrl").

load() ->
	io:format("Loading DB...\n"),
	load_monster_db().

load_monster_db() ->
	{ok, Monsters} = file:consult("monsters.mdb"),
	[load_monster(Prop) || Prop <- Monsters].

load_monster(Prop) ->
	Monster = lists:foldl(fun parse_monster/2, #monster_db{}, Prop),
	mnesia:transaction(fun() -> mnesia:write(Monster) end),
	io:format("monster loaded: ~p\n", [Monster]).

parse_monster({id,     Val}, Mon) -> Mon#monster_db{ id     = Val };
parse_monster({name,   Val}, Mon) -> Mon#monster_db{ name   = Val };
parse_monster({hp,     Val}, Mon) -> Mon#monster_db{ hp     = Val };
parse_monster({level,  Val}, Mon) -> Mon#monster_db{ level  = Val };
parse_monster({damage, Val}, Mon) -> Mon#monster_db{ damage = Val };
parse_monster({loot,   Val}, Mon) -> Mon#monster_db{ loot   = Val };
parse_monster(_, Mon)             -> Mon.