-module(erlmur_client).

-export([handler/1]).

-record(client_state, {socket, session, server_pid}).

handler(Socket) ->
	State = #client_state{socket = Socket},
	get_request(State, []).

get_request(State, BinaryList) ->
	case ssl:recv(State#client_state.socket, 0, 10000) of
	{ok, Binary} ->
		LeftOver = handle_message(State, iolist_to_binary([BinaryList|Binary])),
		get_request(State, LeftOver);
	{error, closed} ->
		io:format("Connection closed ~w~n", [State#client_state.session]);
	{error, timeout} ->
		io:format("Connection timeout ~w~n", [State#client_state.session])
	end.

handle_message(State, << Type:16/unsigned-big-integer, Size:32/unsigned-big-integer, _Message:Size/binary, Rest/binary >>) ->
	io:format("Type: ~w Size: ~w~n", [Type, Size]),
	handle_message(State, Rest);

handle_message(_State, <<>>) ->
	<<>>;

handle_message(_State, Binary) ->
	io:format("Partial message? ~w~n", [Binary]),
	Binary.
