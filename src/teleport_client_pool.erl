-module(teleport_client_pool).

-export([start/3]).

start(Name, Ip, Port) ->
    ok = shackle_pool:start(Name, teleport_client,
                            [{ip, Ip},
                             {port, Port},
                             {reconnect, true},
                             {reconnect_time_max, timer:seconds(2)},
                             {reconnect_time_min, timer:seconds(1)},
                             {socket_options, [binary,
                                               {buffer, 65535},
                                               {nodelay, true},
                                               {packet, 4},
                                               {send_timeout, 5000},
                                               {send_timeout_close, true}
                                              ]}
                            ], [{backlog_size, 1024},
                                {pool_size, 16},
                                {pool_strategy, round_robin}]).
