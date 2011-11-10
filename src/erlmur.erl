-module(erlmur).

-export([start/0, stop/0]).

ensure_started(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error, {already_started, App}} ->
			ok
	end.

%% @spec start() -> ok
%% @doc Start the erlmur server.
start() ->
	ensure_started(crypto),
	ensure_started(public_key),
	ensure_started(ssl),
	application:start(erlmur).

%% @spec stop() -> ok
%% @doc Stop the erlmur server.
stop() ->
	application:stop(erlmur).
