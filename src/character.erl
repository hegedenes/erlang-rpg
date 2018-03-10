-module(character).

-export([visibility_update/1, init_character_process/1]).

-include("db.hrl").

init_character_process(Char) ->
	update_character(Char#character{ pid = self() }, false).

update_character(Char, Waiting) ->
	Res =
	receive
		{move_direction, Direction} ->
			io:format("update_character: move\n"),
			NewPos  = gen_new_pos(Direction, Char#character.pos),
			unit:move_to(Char, NewPos);
		{set_hp, NewHp} ->
			Char#character{ hp = min(Char#character.level * 25, NewHp) };
		{add_hp, Hp} ->
			Char#character{ hp = min(Char#character.level * 25, Char#character.hp + Hp) };
		{lose, _Against} ->
			?pid(Char) ! {add_text, "You die."},
			Char#character{ hp = 25, level = 1, xp = 0 };
		action ->
			#cell{ units = Units, object = Object } = base:get_dyn_cell(Char#character.map, Char#character.pos),
			case {Units, Object} of
				{[Mon = #monster{}|_], _} -> unit:fight(Char, Mon);
				{_, none}  -> ?pid(Char) ! {add_hp, 5};
				_ ->
					?pid(Char) ! {add_item, {Object, 1}},
					base:replace_object(none, Char#character.pos)
			end;
		stop ->
			io:format("update_character: stop\n"),
			save_char(Char),
			base:remove_unit(Char),
			stop;
		{add_item, Item = {Name, Count}} ->
			NewChar = add_item(Char, Item),
			?pid(Char) ! {add_text, "You received " ++ ?s(Count) ++ " [" ++ Name ++ "]."},
			save_char(NewChar),
			NewChar;
		{add_text, Text} ->
			add_text(Char, Text);
		{add_xp, Xp} ->
			?pid(Char) ! {add_text, ?s(Xp) ++ " xp gained."},
			NewXp = Char#character.xp + Xp,
			if
				NewXp >= Char#character.level * 100 ->
					?pid(Char) ! {add_text, "Level up!"}, Char#character{ level = Char#character.level + 1, xp = NewXp - Char#character.level * 100 };
				true -> Char#character{ xp = NewXp }
			end;
		save -> save_char(Char);
		wait -> wait;
		continue -> continue;
		{response_inventory, RespSessionID, Pid} ->
			io:format("update_character: response inventory\n"),
			Response = html:gen_inventory(Char),
			util:response(RespSessionID, Response),
			Pid ! sent;
		{response, Type, RespSessionID, Pid} ->
			io:format("update_character: response\n"),
			case {Waiting, can_respond()} of
				{false, true} ->
					PosChar =
					case Type of
						stats -> util:response(RespSessionID, html:stats(Char)),
								 Char#character{ texts = lists:sublist(Char#character.texts, ?HISTORY_COUNT) };
						map   -> util:response(RespSessionID, html:gen_map(Char));
						base  -> Response = ["<img onload=\"load('/map.html?rand=" ++ util:rand_str(8) ++ "', 'mainframe')\" src=\"empty.gif\" />"],
								 util:response(RespSessionID, Response)
					end,
					Pid ! sent,
					PosChar;
				_ -> self() ! {response, Type, RespSessionID, Pid}
			end
	after 5000 -> save_char(Char)
	end,
	case Res of
		stop         -> ok;
		wait         -> update_character(Res,  true);
		continue     -> update_character(Res,  false);
		#character{} -> update_character(Res,  Waiting);
		_            -> update_character(Char, Waiting)
	end.

can_respond() ->
	List = element(2, erlang:process_info(self(), messages)),
	lists:all(fun(X) -> X end, lists:map(fun({response, _, _, _}) -> true; (_) -> false end, List)).

gen_new_pos("u", {X, Y}) -> {X, Y + 1};
gen_new_pos("d", {X, Y}) -> {X, Y - 1};
gen_new_pos("r", {X, Y}) -> {X + 1, Y};
gen_new_pos("l", {X, Y}) -> {X - 1, Y};
gen_new_pos( _ , {X, Y}) -> {X,     Y}.

save_char(Char) ->
	F = fun() -> mnesia:write(Char) end,
	mnesia:transaction(F).

visibility_update(Char = #character{ visibility = V, pos = {X, Y}}) ->
	V1 = update_vis(V,  {X - 1, Y}, 0.5),
	V2 = update_vis(V1, {X + 1, Y}, 0.5),
	V3 = update_vis(V2, {X,     Y}, 1.0),
	V4 = update_vis(V3, {X, Y + 1}, 0.5),
	Char#character{ visibility = update_vis(V4, {X, Y - 1}, 0.5)}.

update_vis(Visible, Pos, Rate) ->
	case lists:keyfind(Pos, 1, Visible) of
		{_, CurrRate} ->
			lists:keyreplace(Pos, 1, Visible, {Pos, min(CurrRate + Rate, 1)});
		false ->
			[{Pos, Rate} | Visible]
	end.

add_text(Char, Text) -> Char#character{ texts = [Text|Char#character.texts] }.
add_item(Char = #character{ inventory = Inv }, Item = {Name, Count}) ->
	NewInv =
		case lists:keyfind(Name, 1, Inv) of
			{Name, HasCount} ->
				lists:keyreplace(Name, 1, Inv, {Name, HasCount + Count});
			false ->
				[Item|Char#character.inventory]
		end,
	Char#character{ inventory = NewInv }.