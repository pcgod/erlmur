-module(erlmur_server).

-export([start_link/0]).

start_link() ->
	io:format("starting socket server~n"),
	erlmur_socket_server:start(?MODULE, 64747, {erlmur_client, handler}).
