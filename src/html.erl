-module(html).

-export([gen_map/1, gen_login/0, gen_login/1, gen_inventory/1, stats/1]).

-include("db.hrl").

gen_map(Char) ->
    ["<table align=\"center\" style=\"width:" ++ ?s(?CELL_SIZE*?VIS_SIZE*2+1) ++ "px; height:" ++ ?s(?CELL_SIZE*?VIS_SIZE*2+1)++ "px;"
    "border-collapse: collapse; font-size: 50%; text-align:center;\">" %"
    ++ gen_map_cells(Char) ++ "</table>"].

gen_map_cells(Char = #character{ pos = {X, Y}}) ->
    lists:flatten([ gen_line(Char, X, Y + Dy) || Dy <- lists:seq(?VIS_SIZE, -?VIS_SIZE, -1)]).

gen_line(Char, X, Y) ->
    "<tr>" ++ lists:flatten([ gen_cell(Char, X + Dx, Y) || Dx <- lists:seq(-?VIS_SIZE, ?VIS_SIZE)]) ++ "</tr>".

gen_cell(Char = #character{ map = MapID, visibility = _Visible }, X, Y) ->
    % Rate =
    %    case lists:keyfind({X, Y}, 1, Visible) of
    %        {_, CurrRate} -> CurrRate;
    %    false -> 0
    %    end,
    Rate = 1,
    case mnesia:dirty_read(cell_static, {MapID, {X, Y}}) of
        [#cell_static{ type = Type }] ->
            Cell =
                case mnesia:dirty_read(cell, {MapID, {X, Y}}) of
                    [DynCell] -> DynCell;
                    _ -> #cell{ mpos = {MapID, {X, Y}} }
                end,
            {Title, Text, TextColor} = get_cell_info(Cell, Char, Rate),
            Color = get_cell_color(Cell, Type),
            cell(Title, Text, get_color(Rate, Color), TextColor);
        _ ->
            cell("", "", get_color(1, {0,0,util:rand(100, 125)}), "black")
    end.

cell(Title, Text, BgColor, TextColor) ->
    "<td title=\"" ++ Title ++ "\" "
    "style=\"height: " ++ ?s(?CELL_SIZE) ++ "px; width: " ++ ?s(?CELL_SIZE) ++ "px;"
    "background-color: " ++ BgColor ++ "; color: " ++ TextColor ++ ";\">" ++ Text ++ "</td>".

get_cell_color(#cell{ object = Obj }, Type) ->
    case Obj of
        none -> get_cell_type_color(Type);
        _    -> {255,75,0}
    end.

get_cell_type_color({_, Color}) -> Color;
get_cell_type_color(bridge) -> {150, 75, 0};
get_cell_type_color(wall) -> {127, 127, 127};
get_cell_type_color(water) -> {0,0,util:rand(175, 200)};
get_cell_type_color(_) -> {0,0,0}.

get_cell_info(Cell, Char, _) when Cell#cell.mpos == {Char#character.map, Char#character.pos} ->
    {"You", "X", "black"};
get_cell_info(_, _, Rate) when Rate < 0.33 ->
    {"", "", "black"};
get_cell_info(Cell, _, _) ->
    case Cell#cell.units of
        [#monster{ id = ID, level = Level, hp = HP } | _] ->
            Mon = base:get_monster_data(ID),
            Info = Mon#monster_db.name ++ "\nLevel: " ++ ?s(Level) ++ "\nDamage: " ++ ?s(Mon#monster_db.damage) ++ "\nHP: " ++ ?s(HP),
            {Info, ?s(Level), "red"};
        [#character{ name = Name} | _] -> {"Player: " ++ Name, "Y", "yellow"};
        [] ->
            Title =
                case Cell#cell.object of
                    none -> {MapID, {X, Y}} = Cell#cell.mpos, lists:concat([MapID, ":(", X, ",", Y, ")"]);
                    Obj  -> Obj
                end,
            {Title, "", "black"};
        _  -> {"Unknown", "?", "black"}
    end.

get_color(Rate, {R, G, B}) ->
    html_color_str(round(Rate * R), round(Rate * G), round(Rate * B)).

html_color_str(R, G, B) ->
    "#" ++ color_hexstr(R) ++ color_hexstr(G) ++ color_hexstr(B).

color_hexstr(C) ->
    ["0" || C < 16] ++ integer_to_list(C, 16).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stats(Char) ->
    [format_name(Char) ++
    "<table style=\"width: 83%; text-align:center;\">"
        ++ format_stat("HP", bar(Char#character.hp, Char#character.level * 25))
        ++ format_stat("XP", bar(Char#character.xp, Char#character.level * 100)) ++
    "</table>"
    ++ line() ++
    "<table style=\"text-align: left; font-family: Calibri; font-size: 9pt; \">" ++ format_texts(Char#character.texts) ++ "</table>"].

format_name(Char) ->
    "<h3>" ++ Char#character.name ++ " - Level " ++ ?s(Char#character.level) ++ "</h3>".

line() ->
    "<hr style=\"color: #314; background-color: #314; width: 80%;\"/>".

menu() ->
    "<table align=\"center\"><tr>"
    ++ menu_elem(button("Map",       "/script/handle%3Aindex",     25, 75))
    ++ menu_elem(button("Inventory", "/script/handle%3Ainventory", 25, 75))
    ++ menu_elem(button("Logout",    "/script/handle%3Alogin",     25, 75)) ++
    "</tr></table>".

menu_elem(Elem) -> "<td align=\"center\" style=\"width: 75px;\">" ++ Elem ++ "</td>".

button(Title, Action, Height, Width) ->
    "<button onclick=\"load(' " ++ Action ++ " ', 'mainframe')\" style=\"height: " ++ ?s(Height) ++ "px; width: " ++ ?s(Width) ++ "px\" type=\"submit\">" ++ Title ++ "</button>".

format_stat(Key, Value) ->
    "<tr><td>" ++ Key ++ ":</td><td style=\"width: 80%;\">" ++ Value ++ "</td></tr>".

format_texts([]) ->
    "";
format_texts([Text|T]) ->
    "<tr><td>" ++ Text ++ "</td></tr>" ++ format_texts(T).

bar(Val, Max) ->
    Percent = round(100 * Val / Max),
    "<div title=\"" ++ ?s(Val) ++ " / " ++ ?s(Max) ++ "\" style=\"width: 100%; height: 15px; background-color: oldlace;\">"
    "<div style=\"width: " ++ ?s(Percent) ++ "%; height: 15px; background-color: "++bar_color(Percent)++";\">"
    "</div></div>".

bar_color(Percent) when Percent =< 50 ->
    html_color_str(255, round(Percent / 50 * 255), 0);
bar_color(Percent) ->
    html_color_str(round((100 - Percent) / 50 * 255), 255, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_login() ->
    gen_login("").
gen_login(Msg) ->
    ["<div style=\"text-align:center; padding: 1px; margin: 50px auto; width: 300px; background-color: #526C85;\">"
        "<div><h2 style=\"font-family: 'Courier New', Courier, monospace;\">Login</h2></div>"
        "<div style=\"color: black; margin: auto; padding: 5px 0px; width: 70%; background-color: #AAA393;\">"
            "<form>"
                "<table align=\"center\">"
                    "<tr><td><input type=\"text\" name=\"u\" /></tr>"
                    "<tr><td><input type=\"password\" name=\"p\" /></tr>"
                "</table>"
                "<input type=\"button\" value=\"Login\" onClick=\"login(this.form)\" />"
            "</form>"
        "</div>"
        "<p style=\"background-color: #d44;\">" ++ Msg ++ "</p>"
    "</div>"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_inventory(Char) ->
    "<table align=\"center\" style=\"vertical-align:top; width: 90%; height: 100%; text-align:center;\">"
    "<tr><td style=\"width: 50%;\">" ++ gen_equipment_frame(Char) ++ "</td>"
    "<td style=\"width: 50%;\">" ++ gen_items(Char) ++ "</td></tr>"
    "<tr><td></td><td>" ++ menu() ++ "</td></tr>"
    "</table>".

gen_equipment_frame(Char) ->
    "<table align=\"center\"><tr><th style=\"height: 50px;\">Equipment</th></tr><tr><td>" ++ gen_body(Char) ++ "</td></tr></table>".
    
gen_body(Char) ->
    Body = Char#character.equipment,
    "<table border=1 align=\"center\" style=\"vertical-align:top; border: 0px; width: 100%; height: 100%; text-align:center;\">"
    "<tr style=\"height: 50px;\"><td style=\"border: 0px; width: 25px;\"></td><td style=\"width: 50px;\">" ++ equip_str(Body#body.head) ++ "</td><td style=\"border: 0px; width: 25px;\"></td></tr>"
    "<tr style=\"height: 75px;\"><td>" ++ equip_str(Body#body.lhand) ++ "</td><td>" ++ equip_str(Body#body.chest) ++ "</td><td>" ++ equip_str(Body#body.rhand) ++ "</td></tr>"
    "<tr style=\"height: 75px;\"><td style=\"border: 0px;\"></td><td>" ++ equip_str(Body#body.legs) ++ "</td><td style=\"border: 0px;\"></td></tr>"
    "<tr style=\"height: 25px;\"><td style=\"border: 0px;\"></td><td>" ++ equip_str(Body#body.feet) ++ "</td><td style=\"border: 0px;\"></td></tr>"
    "</table>".

equip_str(undefined) -> "-";
equip_str({Name, _}) -> Name.

format_items([]) ->
    "<tr style=\"height: 100%\"><td></td></tr>";
format_items([{Name, Count}|T]) ->
    "<tr><td>" ++ Name ++ " (" ++ ?s(Count) ++ ")</td></tr>" ++ format_items(T).

gen_items(Char) ->
    "<table style=\"vertical-align:top; width: 100%; height: 100%; text-align:left;\">"
    "<tr><th style=\"height: 50px; text-align:center;\">Inventory</th></tr>" ++ format_items(Char#character.inventory) ++ "</table>".
