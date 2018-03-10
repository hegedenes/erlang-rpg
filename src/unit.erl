-module(unit).

-export([move_to/2, interact/2, deinteract/2, fight/2]).
-export([get_pos/1, get_map/1, get_mpos/1, get_guid/1, get_pid/1]).

-include("db.hrl").

can_move_to(_Unit, #cell_static{ mpos = Pos, type = Type }) ->
	moveable_type(Type) andalso world:valid_pos(Pos).

moveable_type({field, _}) -> true;
moveable_type(bridge)     -> true;
moveable_type(_)          -> false.

move_to(Unit, Pos) ->
	case base:get_cell(get_map(Unit), Pos) of
		error -> Unit;
		Cell ->
			case can_move_to(Unit, Cell) of
				true ->
					base:deinteract_cell(Unit),
					base:remove_unit(Unit),
					UnitP = update_pos(Unit, Pos),
					NewUnit = move_update(UnitP),
					base:add_unit(NewUnit),
					base:interact_cell(NewUnit),
					NewUnit;
				false ->
					Unit
			end
	end.

move_update(Char = #character{}) ->
	character:visibility_update(Char);
move_update(Unit) ->
	Unit.

interact(A, B) ->
	case {erlang:process_info(?pid(A)), erlang:process_info(?pid(B))} of
		{undefined, _} -> io:format("interact: A undefined\n");
		{_, undefined} -> io:format("interact: B undefined\n");
		_              -> interact2(A, B)
	end.

interact2(#monster{ pid = _Pid2 }, #monster{ pid = _Pid1 }) ->
	ok;
interact2(#character{ pid = CharPid }, #monster{ pid = MonPid, id = ID }) ->
	MonData = base:get_monster_data(ID),
	CharPid ! {add_text, "You got near [" ++ MonData#monster_db.name ++ "]."},
	MonPid  ! {can_move, false};
interact2(_, _) ->
	ok.

deinteract(A, B) ->
	case {erlang:process_info(?pid(A)), erlang:process_info(?pid(B))} of
		{undefined, _} -> io:format("deinteract: A undefined\n");
		{_, undefined} -> io:format("deinteract: B undefined\n");
		_              -> deinteract2(A, B)
	end.

deinteract2(#monster{ pid = _Pid2 }, #monster{ pid = _Pid1 }) ->
	ok;
deinteract2(#character{ pid = _CharPid }, #monster{ pid = MonPid }) ->
	io:format("deinteract char\n"),
	MonPid  ! {can_move, true};
deinteract2(_, _) ->
	ok.

fight(A, B) ->
	case {erlang:process_info(?pid(A)), erlang:process_info(?pid(B))} of
		{undefined, _} -> io:format("fight: A undefined\n");
		{_, undefined} -> io:format("fight: B undefined\n");
		_              -> fight2(A, B)
	end.

fight2(Char = #character{ pid = CharPid, level = CL }, Mon = #monster{ pid = MonPid, id = ID, level = ML }) ->
	MonData = base:get_monster_data(ID),
	NewHp = Char#character.hp - util:get_range_val(MonData#monster_db.damage),
	if
		NewHp =< 0 ->
			MonPid  ! win,
			CharPid ! {lose, Mon};
		true ->
			MonPid  ! die,
			case monster:xp_gain(CL, ML) of
				0  -> ok;
				XP -> CharPid ! {add_xp, XP}
			end,
			[loot(CharPid, Item) || Item <- MonData#monster_db.loot],
			CharPid ! {set_hp, NewHp}
	end.

loot(CharPid, {Name, Range}) ->
	Count = util:get_range_val(Range),
	case Count of
		0 -> ok;
		_ -> CharPid ! {add_item, {Name, Count}}
	end.

get_pos(#character{ pos = Pos}) -> Pos;
get_pos(  #monster{ pos = Pos}) -> Pos.

get_map(#character{ map = Map}) -> Map;
get_map(  #monster{ map = Map}) -> Map.

get_mpos(#character{ map = Map, pos = Pos}) -> {Map, Pos};
get_mpos(  #monster{ map = Map, pos = Pos}) -> {Map, Pos}.

update_pos(Char  = #character{}, NewPos) -> Char#character{  pos = NewPos };
update_pos(Monster = #monster{}, NewPos) -> Monster#monster{ pos = NewPos }.

get_guid(#character{ guid = GUID }) -> GUID;
get_guid(  #monster{  pid = Pid  }) -> Pid.

get_pid(#character{ pid = Pid }) -> Pid;
get_pid(  #monster{ pid = Pid }) -> Pid;
get_pid(  #session{ pid = Pid }) -> Pid.