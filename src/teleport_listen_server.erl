-module(teleport_listen_server).

-behaviour(gen_server).

%% public api

-export([start_link/0]).

%% gen_server api

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-record(state, {socket :: inet:sock(),
                ref    :: reference(),
                port   :: inet:port()}).

%% public api

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server api

init([]) ->
    AcceptorPoolSize = application:get_env(teleport, acceptor_pool_size, 20),
    % Trapping exit so can close socket in terminate/2
    _ = process_flag(trap_exit, true),
    Opts = [{active, false}, {mode, binary}, {packet, 4}],
    case gen_tcp:listen(0, Opts) of
        {ok, Socket} ->
            % acceptor could close the socket if there is a problem
            MRef = monitor(port, Socket),
            {ok, Port} = inet:port(Socket),
            teleport_acceptor_pool:accept_socket(Socket, AcceptorPoolSize),
            {ok, #state{socket=Socket,
                        ref=MRef,
                        port=Port}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(get_port, _, State=#state{port=Port}) ->
    {reply, {ok, Port}, State};
handle_call(Req, _, State) ->
    {stop, {bad_call, Req}, State}.

handle_cast(Req, State) ->
    {stop, {bad_cast, Req}, State}.

handle_info({'DOWN', MRef, port, Socket, Reason}, State=#state{socket=Socket,
                                                               ref=MRef}) ->
    {stop, Reason, State};
handle_info(_, State) ->
    {noreply, State}.

code_change(_, State, _) ->
    {ok, State}.

terminate(_, #state{socket=Socket,
                    ref=MRef}) ->
    % Socket may already be down but need to ensure it is closed to avoid
    % eaddrinuse error on restart
    case demonitor(MRef, [flush, info]) of
        true  -> gen_tcp:close(Socket);
        false -> ok
    end.
