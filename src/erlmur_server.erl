-module(erlmur_server).
-behaviour(gen_server).

-record(server_state, {max_users, id_list = [], used_ids = []}).

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

handle_call({allocate_session}, {Pid, _Tag}, State) ->
	{NewState, SessionId} = allocate_session(State, Pid),
	io:format("Allocated session id ~w~n", [SessionId]),
	{reply, {session, SessionId}, NewState};

handle_call(_Message, _From, State) ->
	{reply, error, State}.

handle_cast({deallocate_session, Pid}, State) ->
	{NewState, SessionId} = deallocate_session(State, Pid),
	io:format("Deallocated session id ~w~n", [SessionId]),
	{noreply, NewState};

handle_cast(_Message, State) -> {noreply, State}.
handle_info(_Message, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

%% ===================================================================

allocate_session(State, Pid) ->
	[SessionId | Ids] = State#server_state.id_list,
	NewState = State#server_state{id_list = Ids, used_ids = [{Pid, SessionId} | State#server_state.used_ids]},
	{NewState, SessionId}.

deallocate_session(State, Pid) ->
	{value, {_Pid, SessionId}, NewUsed} = lists:keytake(Pid, 1, State#server_state.used_ids),
	NewState = State#server_state{id_list = [SessionId | State#server_state.id_list], used_ids = NewUsed},
	{NewState, SessionId}.
