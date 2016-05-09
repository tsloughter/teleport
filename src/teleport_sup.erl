-module(teleport_sup).

-behaviour(supervisor).

%% public api

-export([start_link/0]).

%% supervisor api

-export([init/1]).

%% public api

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor api

init([]) ->
    Flags = #{strategy => rest_for_one},
    Pool = #{id => teleport_acceptor_pool,
             start => {teleport_acceptor_pool, start_link, []}},
    Socket = #{id => teleport_listen_server,
               start => {teleport_listen_server, start_link, []}},
    {ok, {Flags, [Pool, Socket]}}.
