-module(handle).

-export([login/3, event/3, map/3, stats/3, inventory/3, index/3]).

-include("db.hrl").

get_session_from_env(Env) ->
	case proplists:get_value(http_cookie, Env) of
		undefined -> undefined;
		Cookies ->
			case proplists:get_value("session", httpd:parse_query(Cookies)) of
				undefined -> undefined;
				Session ->
					case base:get_session(Session) of
						error -> undefined;
						RealSession -> RealSession
					end
			end
	end.

index(SessionID, Env, Input) ->
	io:format("index: ~p - ~p\n", [Input, Env]),
	case get_session_from_env(Env) of
		undefined -> util:response(SessionID, html:gen_login());
		RealSession ->
			?pid(RealSession) ! {response, base, SessionID, self()},
			wait_for_response(SessionID)
	end.

login(SessionID, Env, Input) ->
	io:format("login: session: ~p, input: ~p\n", [SessionID, Input]),
	case get_session_from_env(Env) of
		undefined -> ok;
		Session   -> base:delete_session(Session)
	end,
	case httpd:parse_query(Input) of
		[{"u", UserName}, {"p", Password}] ->
			User = base:get_user(UserName),
			io:format("got: ~p\n", [User]),
			case User /= error andalso Password == User#account.password of
				true ->
					io:format("add\n"),
					case base:create_session(User#account.guid) of
						#session{ sessionid = ID, pid = Pid } ->
							util:response(SessionID, ["Content-type: text/html\r\nSet-Cookie: session=" ++ ID ++ "\r\n\r\n"]),
							Pid ! {response, base, SessionID, self()},
							wait_for_response(SessionID);
						error -> util:response(SessionID, html:gen_login("character not found"))
					end;
				false -> util:response(SessionID, html:gen_login("bad username/password"))
			end;
		_ -> util:response(SessionID, html:gen_login())
	end.

stats(SessionID, Env, Input) ->
	io:format("stats: session: ~p, input: ~p\n", [SessionID, Input]),
	case get_session_from_env(Env) of
		error ->
			io:format("stats: invalid session\n"),
			util:response(SessionID, ["invalid session"]);
		Session ->
			?pid(Session) ! {response, stats, SessionID, self()},
			wait_for_response(SessionID)
	end.

map(SessionID, Env, Input) ->
	io:format("map: session: ~p, input: ~p\n", [SessionID, Input]),
	case get_session_from_env(Env) of
		error ->
			io:format("map: invalid session\n"),
			util:response(SessionID, ["invalid session"]);
		Session ->
			?pid(Session) ! {response, map, SessionID, self()},
			wait_for_response(SessionID)
	end.

inventory(SessionID, Env, Input) ->
	io:format("inventory: session: ~p, input: ~p\n", [SessionID, Input]),
	case get_session_from_env(Env) of
		error ->
			io:format("inventory: invalid session\n"),
			util:response(SessionID, html:gen_login("invalid session\n"));
		Session ->
			?pid(Session) ! {response_inventory, SessionID, self()},
			wait_for_response(SessionID)
	end.

event(SessionID, Env, Input) ->
	io:format("event: session: ~p, input: ~p\n", [SessionID, Input]),
	case get_session_from_env(Env) of
		error ->
			io:format("event: invalid session\n"),
			util:response(SessionID, html:gen_login("invalid session\n"));
		Session ->
			handle_params(httpd:parse_query(Input), Session),
			?pid(Session) ! {response, map, SessionID, self()},
			wait_for_response(SessionID)
	end.

wait_for_response(SessionID) ->
	receive
		sent -> ok
	after 2000 ->
		io:format("wait_for_response: timeout\n"),
		Resp = html:gen_login("session timeout\n"),
		util:response(SessionID, Resp)
	end.

handle_params([{"move", Direction}], Session) ->
	Session#session.pid ! {move_direction, Direction};
handle_params([{"action", _Action}], Session) ->
	Session#session.pid ! action;
handle_params([{"msg", Msg}], Session) ->
	case base:get_character(Session#session.guid) of
		#character{ name = Name } ->
			base:send_to_all("<b>[" ++ Name ++ "]</b>: " ++ Msg);
		_ -> ok
	end;
handle_params(_, _) ->
	ok.
