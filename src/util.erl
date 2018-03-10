-module(util).

-export([rand/2, get_range_val/1, rand_pos/0, rand_str/1]).
-export([response/2]).

-include("db.hrl").

response(SessionID, List) ->
	mod_esi:deliver(SessionID, List).

rand(Min, Max) when Max < Min ->
	0;
rand(Min, Max) ->
	Min + random:uniform(Max - Min + 1) - 1.

get_range_val({Min, Max}) -> rand(Min, Max);
get_range_val(Value)      -> Value.

rand_str(Length) ->
	binary_to_list(base64:encode(crypto:rand_bytes(Length))).

rand_pos() ->
	X = rand(-?MAP_SIZE, ?MAP_SIZE),
	Y = rand(-?MAP_SIZE, ?MAP_SIZE),
	{X, Y}.
