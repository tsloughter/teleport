-module(teleport_broker).

-behaviour(sbroker).

-export([start_link/1]).

-export([init/1]).

%% TODO: remove me when fixed
-dialyzer({nowarn_function, start_link/1}).

start_link(Name) ->
    sbroker:start_link({local, teleport:name_for_node(Name)}, ?MODULE, [], [{read_time_after, 16}]).

init(_) ->
    QueueSpec = {sbroker_timeout_queue, {out, 5000, drop, 128}},
    WorkerSpec = {sbroker_drop_queue, {out_r, drop, infinity}},
    {ok, {QueueSpec, WorkerSpec, 200}}.
