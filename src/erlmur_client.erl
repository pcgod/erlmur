-module(erlmur_client).

-include("mumble_pb.hrl").

-export([handler/2]).

-define(MSG_VERSION, 0).
-define(MSG_UDPTUNNEL, 1).
-define(MSG_AUTHENTICATE, 2).
-define(MSG_PING, 3).
-define(MSG_REJECT, 4).
-define(MSG_SERVERSYNC, 5).
-define(MSG_CHANNELREMOVE, 6).
-define(MSG_CHANNELSTATE, 7).
-define(MSG_USERREMOVE, 8).
-define(MSG_USERSTATE, 9).
-define(MSG_BANLIST, 10).
-define(MSG_TEXTMESSAGE, 11).
-define(MSG_PERMISSONDENIED, 12).
-define(MSG_ACL, 13).
-define(MSG_QUERYUSERS, 14).
-define(MSG_CRYPTSETUP, 15).
-define(MSG_CONTEXTACTIONADD, 16).
-define(MSG_CONTEXTACTION, 17).
-define(MSG_USERLIST, 18).
-define(MSG_VOICETARGET, 19).
-define(MSG_PERMISSIONQUERY, 20).
-define(MSG_CODECVERSION, 21).
-define(MSG_USERSTATA, 22).
-define(MSG_REQUESTBLOB, 23).
-define(MSG_SERVERCONFIG, 24).
-define(MSG_SUGGESTCONFIG, 25).

-record(client_state, {socket, session, server_pid, authenticated = false}).

handler(ServerPid, Socket) ->
	{session, SessionId} = gen_server:call(ServerPid, {client_connected}),
	State = #client_state{socket = Socket, session = SessionId, server_pid = ServerPid},
	get_request(State, []).

get_request(State, BinaryList) ->
	case ssl:recv(State#client_state.socket, 0, 30000) of
	{ok, Binary} ->
		LeftOver = handle_message(State, iolist_to_binary([BinaryList | Binary])),
		get_request(State, LeftOver);
	{error, closed} ->
		io:format("Connection closed ~w~n", [State#client_state.session]),
		gen_server:cast(State#client_state.server_pid, {client_disconnected, self()}),
		{stop, normal, State};
	{error, timeout} ->
		io:format("Connection timeout ~w~n", [State#client_state.session]),
		gen_server:cast(State#client_state.server_pid, {client_disconnected, self()}),
		{stop, normal, State}
	end.

handle_message(State, << Type:16/unsigned-big-integer, Size:32/unsigned-big-integer, Message:Size/binary, Rest/binary >>) ->
	io:format("Type: ~w Size: ~w~n", [Type, Size]),
	handle_protobuf_message(State, Type, Message),
	handle_message(State, Rest);

handle_message(_State, <<>>) ->
	<<>>;

handle_message(_State, Binary) ->
	io:format("Partial message? ~w~n", [Binary]),
	Binary.

send_message(State, Type, Message) ->
	Size = byte_size(Message),
	ssl:send(State#client_state.socket, << Type:16/unsigned-big-integer, Size:32/unsigned-big-integer, Message/binary >>).

%% ===================================================================
%% Protobuf message handlers
%% ===================================================================

handle_protobuf_message(_State, ?MSG_VERSION, Message) ->
	Version = mumble_pb:decode_version(Message),
	io:format("Version: ~p~n", [Version]);

handle_protobuf_message(State,?MSG_AUTHENTICATE, Message) ->
	Authenticate = mumble_pb:decode_authenticate(Message),
	io:format("Authenticate: ~p~n", [Authenticate]),
	ChannelState = mumble_pb:encode_channelstate(#channelstate{ channel_id = 0, name = "Root" }),
	send_message(State, ?MSG_CHANNELSTATE, ChannelState),
	UserState = mumble_pb:encode_userstate(#userstate{ session = State#client_state.session, name = Authenticate#authenticate.username }),
	send_message(State, ?MSG_USERSTATE, UserState),
	ServerSync = mumble_pb:encode_serversync(#serversync{ session = State#client_state.session, max_bandwidth = 240000 }),
	send_message(State, ?MSG_SERVERSYNC, ServerSync);

handle_protobuf_message(State, ?MSG_PING, Message) ->
	_Ping = mumble_pb:decode_ping(Message),
	%% io:format("Ping: ~p~n", [Ping]),
	send_message(State, ?MSG_PING, Message);

handle_protobuf_message(_State, ?MSG_PERMISSIONQUERY, _Message) ->
	io:format("PermissionQuery~n");

handle_protobuf_message(_State, Type, _Message) ->
	io:format("Unhandled message type ~w~n", [Type]).
