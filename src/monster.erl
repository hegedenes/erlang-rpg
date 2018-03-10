-module(monster).

-behaviour(gen_server).

-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([xp_gain/2]).

-include("db.hrl").

start(Args) ->
   gen_server:start(monster, Args, []).

init(Args) ->
	random:seed(now()),
	ID = proplists:get_value(id, Args, 0),
	MapID = proplists:get_value(map, Args, 0),
	MonData = base:get_monster_data(ID),
	Level = util:get_range_val(MonData#monster_db.level),
	Monster = #monster{
			pid = self(), id = ID, map = MapID,
			hp  = hp_formula(Level, none),
			level = Level,
			pos = proplists:get_value(pos, Args, util:rand_pos())
		},
	base:add_unit(Monster),
	Map = base:get_map(MapID),
	Map#map.pid ! {add, self()},
	process_flag(trap_exit, true),
    {ok, Monster, 2000}.

terminate(Reason, Monster) ->
	io:format("monster:terminate ~p ~p\n", [Reason, self()]),
	Map = base:get_map(Monster#monster.map),
	Map#map.pid ! {remove, Monster#monster.pid},
	base:remove_unit(Monster).

code_change(OldVsn, State, Extra) ->
	io:format("monster:code_change: ~p extra: ~p", [OldVsn, Extra]),
	{ok, State}.

handle_call(Msg, _From, Monster) ->
    io:format("monster msg: ~p\n", [Msg]),
    {noreply, Monster}.

handle_cast(Msg, Monster) ->
    io:format("monster msg: ~p\n", [Msg]),
    {noreply, Monster}.

handle_info(timeout, Monster) ->
	NewMon =
		case Monster#monster.can_move of
			false -> io:format("cant move\n"), Monster;
			true  -> move(Monster)
		end,
    nr(NewMon);
handle_info({can_move, Bool}, Monster) ->
    io:format("monster handle_info: can move ~p\n", [Bool]),
    nr(Monster#monster{ can_move = Bool });
handle_info(die, Monster) ->
    {stop, normal, Monster};
handle_info(win, Monster) ->
    nr(Monster);
handle_info(Info, Monster) ->
    io:format("monster handle_info: ~p\n", [Info]),
    nr(Monster).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nr(Monster) -> {noreply, Monster, 2000}.

xp_gain(PL, ML) ->
	Diff = ML - PL,
	if Diff < -2 -> 0;
	   true      -> (Diff + 3) * 5
	end.

hp_formula(Level, _Type) ->
	Level * 10 + util:rand(-5,5).

move(Monster = #monster{ pos = {X, Y} }) ->
	NewPos = {X + util:rand(-1, 1), Y + util:rand(-1, 1)},
	unit:move_to(Monster, NewPos).