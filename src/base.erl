-module(base).

-export([start/0, stop/0, restart/0, init_db/0, compile/0]).
-export([get_cell/2, get_cell/3, get_dyn_cell/2, get_dyn_cell/3, get_monster_data/1, get_monster_ids/0]).
-export([add_user/3, get_user/1, get_character/1, interact_cell/1, deinteract_cell/1]).
-export([add_unit/1, remove_unit/1, replace_object/2, send_to_all/1, delete_session/1]).
-export([create_session/1, get_character_session/1, get_session/1, save_cells/1, save_map/1, get_map/1]).

-include("db.hrl").

compile() ->
	c:c(character), c:c(unit), c:c(map),
	c:c(monster), c:c(world), c:c(handle),
	c:c(html), c:c(db_loader), c:c(base),
	c:c(util),
	ok.

-define(create_table(Name, Save), mnesia:create_table(Name, [{disc_copies, (case Save of true -> [node()]; false -> [] end)}, {attributes, record_info(fields, Name)}])).

init_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    ?create_table(account,     true),
    ?create_table(character,   true),
    ?create_table(cell_static, true),
    ?create_table(map,         true),
    ?create_table(monster_db,  false),
    ?create_table(cell,        false),
    ?create_table(session,     false),
    wait_for_tables(),
	world:generate_map(),
	add_user("a", "a", 0),
	add_user("b", "b", 1).

start_db() -> mnesia:start(), wait_for_tables().

wait_for_tables() -> mnesia:wait_for_tables([account, character, cell_static, map, cell, session], 60000).

start() ->
	io:format("Starting Server...\n"),
	start_db(),
	db_loader:load(),
	inets:start(),
	start_server(),
	map:start([{id, 0}]),
	io:format("Started.\n").

stop() ->
	catch mnesia:stop(),
	catch inets:stop().

restart() -> stop(), start().

start_server() ->
	inets:start(httpd, [
		{modules, [mod_alias, mod_esi, mod_actions, mod_cgi, mod_dir, mod_get, mod_head, mod_log, mod_disk_log]},
		{bind_address, {0,0,0,0}},
		{port, 8081},
		{server_name, "test"},
		{server_root, "data"},
		{document_root, "data/www"},
		{directory_index, ["index.html"]},
		{erl_script_alias, {"/script", [handle]}},
		{error_log, "error.log"},
		{security_log, "security.log"},
		{transfer_log, "transfer.log"},
		{mime_types,[
			{"html","text/html"},
			{"css","text/css"},
			{"js","application/x-javascript"}
		]}
	]).

add_user(UserName, Password, GUID) ->
	CharBase = #character{ name = UserName, guid = GUID },
	User = #account{ username = UserName, password = Password, guid = GUID},
	Char = character:visibility_update(CharBase),
	F = fun() -> mnesia:write(User), mnesia:write(Char) end,
	mnesia:transaction(F).
	
create_session(GUID) ->
	case base:get_character(GUID) of
		error -> io:format("create_session: no char\n"), error;
		Character ->
			SessionID = util:rand_str(9),
			Pid = spawn(character, init_character_process, [Character]),
			Session = #session{ sessionid = SessionID, guid = GUID, pid = Pid },
			F = fun() ->
					OldSessions = mnesia:match_object(#session{ sessionid = '_', guid = GUID, pid = '_'}),
					[delete_session(OldSession) || OldSession <- OldSessions],
					mnesia:write(Session)
				end,
			mnesia:transaction(F),
			Session
	end.
	
delete_session(Session) ->
	io:format("delete_session\n"),
	Session#session.pid ! stop,
	mnesia:transaction(fun() -> mnesia:delete_object(Session) end).

get_user(UserName) ->
	case mnesia:transaction(fun() -> mnesia:read(account, UserName) end) of
		{atomic, [User]} -> User;
		_ -> error
	end.

get_session(SessionID) ->
	case mnesia:transaction(fun() -> mnesia:read(session, SessionID) end) of
		{atomic, [Session]} -> Session;
		_ -> error
	end.

get_character_session(SessionID) ->
	case mnesia:transaction(fun() -> mnesia:read(session, SessionID) end) of
		{atomic, [Session]} -> get_character(Session#session.guid);
		_ -> error
	end.

get_character(GUID) ->
	case mnesia:transaction(fun() -> mnesia:read(character, GUID) end) of
		{atomic, [Char]} -> Char;
		_ -> io:format("Char ~p not found\n", [GUID]), error
	end.

send_to_all(Msg) ->
	F = fun() -> mnesia:foldl(fun(Session, _) -> ?pid(Session) ! {add_text, Msg} end, [], session) end,
	mnesia:transaction(F).

save_cells(Cells) ->
	mnesia:transaction(fun() -> [mnesia:write(Cell) || Cell <- Cells, world:valid_pos(Cell#cell_static.mpos)] end).

save_map(Map) ->
	mnesia:transaction(fun() -> mnesia:write(Map) end).

get_map(ID) ->
	case mnesia:transaction(fun() -> mnesia:read(map, ID) end) of
		{atomic, [Map]} -> Map;
		_ -> error
	end.

get_cell(Map, {X, Y}) ->
	get_cell(Map, X, Y).
get_cell(Map, X, Y) ->
	case mnesia:transaction(fun() -> read_static_cell(Map, {X, Y}) end) of
		{atomic, [Cell]} -> Cell;
		_ -> error
	end.

get_dyn_cell(Map, {X, Y}) ->
	get_dyn_cell(Map, X, Y).
get_dyn_cell(Map, X, Y) ->
	case mnesia:transaction(fun() -> mnesia:read(cell, {Map, {X, Y}}) end) of
		{atomic, [Cell]} -> Cell;
		_ -> #cell{ mpos = {Map, {X, Y}} }
	end.

get_monster_data(#monster{ id = ID }) ->
	get_monster_data(ID);
get_monster_data(ID) ->
	case mnesia:transaction(fun() -> mnesia:read(monster_db, ID) end) of
		{atomic, [Mon]} -> Mon;
		_ -> #monster_db{}
	end.

get_monster_ids() ->
	case mnesia:transaction(fun() -> mnesia:all_keys(monster_db) end) of
		{atomic, List} -> List;
		_ -> []
	end.
	
remove_unit(Unit) ->
	Guid = unit:get_guid(Unit),
	F = fun() ->
			case read_cell(Unit) of
				[Cell = #cell{ units = Units }] ->
					NewUnits = lists:keydelete(Guid, 2, Units),
					if NewUnits == [] andalso Cell#cell.object == none ->
						mnesia:delete_object(Cell);
					   true ->
						mnesia:write(Cell#cell{units = NewUnits})
					end;
				_ -> ok
			end
		end,
	mnesia:transaction(F).

add_unit(Unit) ->
	Guid = unit:get_guid(Unit),
	Mpos = unit:get_mpos(Unit),
	F = fun() ->
			case read_cell(Unit) of
				[Cell = #cell{ units = Units }] ->
					mnesia:write(Cell#cell{units = lists:keystore(Guid, 2, Units, Unit)});
				_ ->
					mnesia:write(#cell{ mpos = Mpos, units = [Unit] })
			end
		end,
	mnesia:transaction(F).

replace_object(Obj, Pos) ->
	Mpos = {0, Pos},
	F = fun() ->
			case mnesia:read(cell, Mpos) of
				[Cell] -> mnesia:write(Cell#cell{ object = Obj });
				_ -> mnesia:write(#cell{ mpos = Mpos, object = Obj })
			end
		end,
	mnesia:transaction(F).

interact_cell(Unit) ->
	case mnesia:transaction(fun() -> read_cell(Unit) end) of
		{atomic, [Cell]} -> interact_cell(Unit, Cell);
		_ -> io:format("interact_cell: Cell not found\n")
	end.

interact_cell(Who, #cell{ units = Units }) ->
	[unit:interact(Who, Unit) || Unit <- Units, unit:get_guid(Who) /= unit:get_guid(Unit)].

deinteract_cell(Unit) ->
	case mnesia:transaction(fun() -> read_cell(Unit) end) of
		{atomic, [Cell]} -> deinteract_cell(Unit, Cell);
		_ -> io:format("deinteract_cell: Cell not found\n")
	end.

read_cell(Unit) -> mnesia:read(cell, unit:get_mpos(Unit)).
read_static_cell(Map, Pos) -> mnesia:read(cell_static, {Map, Pos}).

deinteract_cell(Who, #cell{ units = Units }) ->
	[unit:deinteract(Who, Unit) || Unit <- Units, unit:get_guid(Who) /= unit:get_guid(Unit)].