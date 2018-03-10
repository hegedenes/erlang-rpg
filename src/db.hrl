-ifndef(db_hrl).
-define(db_hrl, true).

-define(s(X), lists:flatten(io_lib:format("~p", [X]))).
-define(pid(U), unit:get_pid(U)).

-define(MAP_SIZE,  50).
-define(VIS_SIZE,  9).
-define(CELL_SIZE, 15).

-define(HISTORY_COUNT, 10).

-define(MONSTER_COUNT, 250).
-define(FLOWER_COUNT, 20).

-record(map, {id, pid, type, size = ?MAP_SIZE}).
-record(cell_static, {mpos, type = {field, {0, 0, 0}}}).
-record(monster_db, {id = 0, name = "Unknown", hp = 1, level = 0, damage = 0, loot = []}).
-record(object_db, {id = 0, type = pickable, name = "Default Object", loot = []}).

-record(account, {username, password, guid}).
-record(body, {head, chest, lhand, rhand, legs, feet}).
-record(character, {guid, pid, name, map = 0, pos = {0, 0}, hp = 25, level = 1, xp = 0,
                    texts = [], inventory = [], equipment = #body{}, visibility = [], state = field}).
-record(cell, {mpos, object = none, units = []}).
-record(session, {sessionid, guid, pid}).
-record(monster, {pid, id = 0, map = 0, pos, level = 0, hp = 100, can_move = true}).

-endif.
