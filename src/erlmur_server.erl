-module(erlmur_server).
-behaviour(gen_server).

-include("mumble_pb.hrl").

-record(client, {
	pid :: pid(),
	session :: integer(),
	username :: string(),
	channel = 0 :: integer()
}).

-record(server, {
	max_users :: integer(),
	max_bandwidth :: integer(),
	id_list :: [integer()],
	clients = [] :: [#client{}] | []
}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0]).

-define(PORT, 64747).
-define(MAX_USERS, 10).
-define(MAX_BANDWIDTH, 240000).

start_link() ->
	io:format("starting socket server~n"),
	State = #server{max_users = ?MAX_USERS, max_bandwidth = ?MAX_BANDWIDTH, id_list = lists:seq(1, ?MAX_USERS)},
	{'ok', Pid} = gen_server:start_link({local, server1}, ?MODULE, State, []),
	erlmur_socket_server:start(server1_socket, ?PORT, {erlmur_client, handler, Pid}).

init(State) ->
	{ok, State}.

handle_call({client_connected}, {Pid, _Tag}, State) ->
	{NewState, SessionId} = client_connected(State, Pid),
	io:format("New client connection [Id: ~w]~n", [SessionId]),
	{reply, {session, SessionId}, NewState};

handle_call(_Message, _From, State) ->
	{reply, error, State}.

handle_cast({authenticate, Pid, {UserName, _Password}}, State) ->
	{ok, #client{session = SessionId} = ClientState} = get_client(State, Pid),
	{_, NewState} = update_client(State, ClientState#client{username = UserName}),
	gen_server:cast(Pid, {send_channelstate, #channelstate{ channel_id = 0, name = "Root" }}),

	[gen_server:cast(Pid, {send_userstate, build_initial_userstate(C)}) || C <- NewState#server.clients, C#client.session /= SessionId],

	gen_server:cast(Pid, {send_userstate, #userstate{ session = SessionId, name = UserName }}),
	gen_server:cast(Pid, {send_serversync, #serversync{ session = SessionId, max_bandwidth = State#server.max_bandwidth }}),

	broadcast_message(NewState, {send_userstate, #userstate{session = SessionId, name = UserName}}, [SessionId]),

	{noreply, NewState};

handle_cast({client_disconnected, Pid}, State) ->
	{ok, #client{session = SessionId} = ClientState} = get_client(State, Pid),
	broadcast_message(State, {send_userremove, #userremove{session = SessionId}}, [SessionId]),
	NewState = client_disconnected(State, ClientState),
	io:format("Closed client connection [Id: ~w]~n", [SessionId]),
	{noreply, NewState};

handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% ===================================================================

-spec get_client(State :: #server{}, Pid :: pid()) -> {ok, #client{}} | {error, notfound}.

get_client(#server{clients = Clients}, Pid) ->
	case [C || C = #client{pid = CPid} <- Clients, CPid == Pid] of
	[] ->
		{error, notfound};
	[C | _] ->
		{ok, C}
	end.

-spec update_client(State :: #server{}, Client :: #client{}) -> {#client{}, #server{}}.

update_client(#server{clients = Clients} = State, #client{session = Session} = Client) ->
	Others = [C || C = #client{session = CSession} <- Clients, CSession /= Session],
	{Client, State#server{clients = [Client | Others]}}.

-spec client_connected(State :: #server{}, Pid :: pid()) -> {#server{}, integer()}.

client_connected(State, Pid) ->
	[SessionId | Ids] = State#server.id_list,
	NewClient = #client{pid = Pid, session = SessionId},
	NewState = State#server{id_list = Ids, clients = [NewClient | State#server.clients]},
	{NewState, SessionId}.

-spec client_disconnected(#server{}, #client{}) -> #server{}.

client_disconnected(#server{id_list = IdList, clients = Clients} = State, #client{pid = Pid, session = Session}) ->
	NewClients = [C || C = #client{pid = CPid} <- Clients, CPid /= Pid],
	State#server{id_list = [Session | IdList], clients = NewClients}.

-spec build_initial_userstate(#client{}) -> #userstate{}.

build_initial_userstate(#client{session = SessionId, username = UserName}) ->
	#userstate{session = SessionId, name = UserName}.

broadcast_message(#server{clients = Clients}, Msg, Except) when is_list(Except) ->
	[gen_server:cast(Pid, Msg) || #client{pid = Pid} <- Clients, lists:member(Pid, Except) == false].
