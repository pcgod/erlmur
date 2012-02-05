-module(erlmur_server).
-behaviour(gen_server).

-record(server_state, {max_users, id_list = [], clients = []}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0]).

-define(PORT, 64747).
-define(MAX_USERS, 10).

start_link() ->
	io:format("starting socket server~n"),
	State = #server_state{max_users = ?MAX_USERS, id_list = lists:seq(1, ?MAX_USERS)},
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

handle_cast({client_disconnected, Pid}, State) ->
	{NewState, SessionId} = client_disconnected(State, Pid),
	io:format("Closed client connection [Id: ~w]~n", [SessionId]),
	{noreply, NewState};

handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% ===================================================================

client_connected(State, Pid) ->
	[SessionId | Ids] = State#server_state.id_list,
	NewState = State#server_state{id_list = Ids, clients = [{Pid, SessionId} | State#server_state.clients]},
	{NewState, SessionId}.

client_disconnected(State, Pid) ->
	{value, {_Pid, SessionId}, NewClients} = lists:keytake(Pid, 1, State#server_state.clients),
	NewState = State#server_state{id_list = [SessionId | State#server_state.id_list], clients = NewClients},
	{NewState, SessionId}.
