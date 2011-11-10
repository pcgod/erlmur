-module(erlmur_socket_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([accept_loop/1]).
-export([start/3]).

-define(SSL_OPTIONS, [binary, {nodelay, true}, {active, false}, {reuseaddr, true}, {certfile, "server.pem"}, {versions, [tlsv1]}]).

-record(acceptor_state, {
	port,
	loop,
	socket = null}
).

start(Name, Port, Loop) ->
	io:format("Starting server on ~w~n", [Port]),
	State = #acceptor_state{port = Port, loop = Loop},
	gen_server:start_link({local, Name}, ?MODULE, State, []).

init(State = #acceptor_state{port = Port}) ->
	case ssl:listen(Port, ?SSL_OPTIONS) of
		{ok, Socket} ->
			NewState = State#acceptor_state{socket = Socket},
			{ok, accept(NewState)};
		{error, Reason} ->
			{stop, Reason}
	end.

handle_cast({accepted, _Pid}, State = #acceptor_state{}) ->
	{noreply, accept(State)}.

accept_loop({Server, Socket, {Mod, Func}}) ->
	io:format("Waiting for connections...~n"),
	{ok, SslSocket} = ssl:transport_accept(Socket),
		gen_server:cast(Server, {accepted, self()}),
		ssl:ssl_accept(SslSocket),
		Mod:Func(SslSocket).

accept(State = #acceptor_state{socket = Socket, loop = Loop}) ->
	proc_lib:spawn(?MODULE, accept_loop, [{self(), Socket, Loop}]),
	State.

handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
