-module(teleport_broker_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Node) ->
    {Server, Port} = teleport:ip_and_port(Node),
    supervisor:start_link(?MODULE, [Node, {Server, Port}]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Name, {Server, Port}]) ->
    Broker = {teleport_broker, {teleport_broker, start_link, [Name]},
              permanent, 5000, worker, [teleport_broker]},
    Workers = [{{teleport_conn, X}, {teleport_conn, start_link, [Name, Server, Port]},
                permanent, 5000, worker, [teleport_conn]} || X <- lists:seq(1, 10)],

    {ok, {{one_for_one, 5, 10}, [Broker | Workers]}}.

%%====================================================================
%% Internal functions
%%====================================================================
