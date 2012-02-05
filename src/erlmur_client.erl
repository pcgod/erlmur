-module(erlmur_client).
-behaviour(gen_server).

-include("mumble_pb.hrl").

-export([handler/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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

-record(client_state, {
	socket :: tuple(),
	server_pid :: pid(),
	message_buffer = [] :: binary() | []
}).

handler(ServerPid, Socket) ->
	State = #client_state{socket = Socket, server_pid = ServerPid},
	gen_server:start_link(?MODULE, State, []).

init(#client_state{socket = Socket, server_pid = ServerPid} = State) ->
	io:format("Client on ~w~n", [self()]),
	ssl:controlling_process(Socket, self()),
	ssl:ssl_accept(Socket),
	{session, _SessionId} = gen_server:call(ServerPid, {client_connected}),
	ssl:setopts(Socket, [{active, once}]),
	{ok, State}.

handle_call(_Message, _From, State) ->
	{reply, error, State}.

handle_cast(_Message, State) -> {noreply, State}.

%% ===================================================================
%% SSL receiving
%% ===================================================================

handle_info({ssl, _S, Binary}, #client_state{message_buffer = MsgBuffer} = State) ->
	NewState = State#client_state{message_buffer = handle_message(State, iolist_to_binary([MsgBuffer | Binary]))},
	ssl:setopts(State#client_state.socket, [{active, once}]),
	{noreply, NewState};

handle_info({ssl_closed, _S}, State) ->
	gen_server:cast(State#client_state.server_pid, {client_disconnected, self()}),
	{stop, normal, State};

handle_info({ssl_error, _S, Reason}, State) ->
	io:format("~w Connection error: ~w~n", [self(), Reason]),
	gen_server:cast(State#client_state.server_pid, {client_disconnected, self()}),
	{stop, normal, State};

handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% ===================================================================
%% Message handling
%% ===================================================================

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
