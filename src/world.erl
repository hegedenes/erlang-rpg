-module(world).

-export([generate_map/0, valid_pos/1, valid_pos/2, generate_lakes/0, generate_rocks/0, gen_growing/2, post_mods/0]).

-include("db.hrl").

generate_map() ->
    io:format("Generating map"),
    case base:get_map(0) of
        error -> ok;
        OldMap -> OldMap#map.pid ! stop
    end,
    generate_field(?MAP_SIZE),
    % generate_circle(5),
    generate_rocks(),
    generate_rivers(),
    generate_lakes(),
    io:format("\nDone\n").

post_mods() ->
    io:format("\Post modifications"),
    F = fun() -> mnesia:foldl(fun(Cell, _) -> check_water(Cell) end, [], cell_static) end,
    mnesia:transaction(F).

check_water(Cell = #cell_static{ mpos = {_, {X, Y}}, type = {field, _} }) ->
    Nearby = has_water({X - 1, Y}) orelse has_water({X + 1, Y})
      orelse has_water({X, Y - 1}) orelse has_water({X, Y + 1}),
    if Nearby -> mnesia:write(Cell#cell_static{ type = {field, {48 + random:uniform(16), 144 + random:uniform(16), 0}} });
       true -> ok
    end;
check_water(_) ->
    ok.
    
has_water(Pos) ->
    case mnesia:read(cell_static, Pos) of
        [Cell] -> Cell#cell_static.type == water;
        [] -> true
    end.

generate_field(R) ->
    io:format("\nGenerating field"),
    Field = [generate(grass, X, Y) || X <- lists:seq(-R, R), Y <- lists:seq(-R, R)],
    base:save_cells(Field).

generate_rivers() ->
    io:format("\nGenerating rivers"),
    Rivers =
    generate_river({-?MAP_SIZE,  random:uniform(?MAP_SIZE)}, 2 * ?MAP_SIZE + random:uniform(?MAP_SIZE), 0) ++
    generate_river({-?MAP_SIZE, -random:uniform(?MAP_SIZE)}, 2 * ?MAP_SIZE + random:uniform(?MAP_SIZE), 1) ++
    generate_river({ ?MAP_SIZE,  random:uniform(?MAP_SIZE)}, 2 * ?MAP_SIZE + random:uniform(?MAP_SIZE), 2) ++
    generate_river({ ?MAP_SIZE, -random:uniform(?MAP_SIZE)}, 2 * ?MAP_SIZE + random:uniform(?MAP_SIZE), 3),
    base:save_cells(Rivers).

generate_river(_, 0, _) ->
    [];
generate_river({X, Y}, C, Angle) ->
    Next =
        case random:uniform(10) of
            1 -> [generate(bridge, X, Y)];
            _ -> [generate(water, X, Y)]
        end,
    Next ++ generate_river(get_next_river_pos(X, Y, Angle), C - 1, Angle).

get_next_river_pos(X, Y, 0) ->
    {X - 1 + random:uniform(2), Y + 1 - random:uniform(2)};
get_next_river_pos(X, Y, 1) ->
    {X - 1 + random:uniform(2), Y - 1 + random:uniform(2)};
get_next_river_pos(X, Y, 2) ->
    {X + 1 - random:uniform(2), Y + 1 - random:uniform(2)};
get_next_river_pos(X, Y, 3) ->
    {X + 1 - random:uniform(2), Y - 1 + random:uniform(2)}.

generate_lakes() ->
    io:format("\nGenerating lakes"),
    Lakes = gen_n(10, fun generate_lake/0),
    base:save_cells(Lakes).

generate_lake() ->
    gen_growing(water, 1).

generate_rocks() ->
    io:format("\nGenerating rocks"),
    Rocks = gen_n(50, fun generate_rock/0),
    base:save_cells(Rocks).

generate_rock() ->
    {X, Y} = util:rand_pos(),
    [generate(wall, X, Y)].

% generate_circle(R) ->
%     [generate(wall, X, Y) || X <- lists:seq(-R, R), Y <- lists:seq(-R, R), abs(X*X + Y*Y - R*R) =< R ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gen_growing(Type, Rate) ->
    Pos = util:rand_pos(),
    io:format("\nGrowing generation ~p ~p\n", [Type, Pos]),
    FailRate = 0.0,
    grow_to(Type, Pos, FailRate, Rate).

grow_to(Type, {X,Y}, FailRate, Rate) ->
    [generate(Type, X, Y)] ++
    case random:uniform() < FailRate of
        true  -> io:format("e"), [];
        false -> io:format("g"),
                 grow_to(Type, {X, Y - 1}, FailRate + random:uniform() / Rate, Rate) ++
                 grow_to(Type, {X - 1, Y}, FailRate + random:uniform() / Rate, Rate) ++
                 grow_to(Type, {X + 1, Y}, FailRate + random:uniform() / Rate, Rate) ++
                 grow_to(Type, {X, Y + 1}, FailRate + random:uniform() / Rate, Rate)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate(grass, X, Y) -> #cell_static{ mpos = {0, {X, Y}}, type = {field, {48 + random:uniform(16), 160 + random:uniform(16), 32}} };
generate(Type, X, Y)  -> #cell_static{ mpos = {0, {X, Y}}, type = Type }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

valid_pos({_, {X, Y}}) -> valid_pos(X, Y);
valid_pos({X, Y}) -> valid_pos(X, Y).
valid_pos( X, Y ) -> abs(X) =< ?MAP_SIZE andalso abs(Y) =< ?MAP_SIZE.

gen_n(0,  _ ) -> [];
gen_n(N, Fun) -> Fun() ++ gen_n(N - 1, Fun).
