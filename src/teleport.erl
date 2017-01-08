-module(teleport).

-export([send/2, connect/1, gs_call/3, start/0, term_to_iolist/1, ip_and_port/1, name_for_node/1]).

start() ->
  application:ensure_all_started(teleport).

connect(Nodes) when is_list(Nodes) ->
    [connect(N) || N <- Nodes];
connect(Node) ->
    teleport_sup:start_child(Node).

send(Process, Message) ->
    Node = get_node(Process),
    case node_addressable(Node) of
        false ->
            {error, nodedown};
        _ ->
            do_send(Process, Node, Message),
            ok
    end.

gs_call(Process, Message, Timeout) ->
    Node = get_node(Process),
    case node_addressable(Node) of
        false ->
            exit({nodedown, Node});
        _ ->
            Mref = erlang:monitor(process, Process),
            _Res = do_send(Process, Node, {'$gen_call', {self(), Mref}, Message}),
            receive
                {Mref, Reply} ->
                    erlang:demonitor(Mref, [flush]),
                    {ok, Reply};
                {'DOWN', Mref, _, _, noconnection} ->
                    exit({nodedown, Node});
                {'DOWN', Mref, _, _, Reason} ->
                    exit(Reason)
            after Timeout ->
                    erlang:demonitor(Mref, [flush]),
                    exit(timeout)
            end
    end.

node_addressable(Node) ->
  case lists:member(Node, nodes()) of
    true ->
      true;
    _ ->
      pong == net_adm:ping(Node)
  end.

do_send(Process, Name, Msg) ->
    {ok, Conn={Socket, _, _}} = get_conn(name_for_node(Name)),
    gen_tcp:send(Socket, term_to_iolist({send, get_dest(Process), Msg})),
    teleport_conn:done(Conn).

get_conn(Name) ->
    case sbroker:ask(Name) of
        {go, Ref, {Pid, Conn}, _RelativeTime, _SojournTime} ->
            {ok, {Conn, Pid, Ref}};
        {drop, _N} ->
            {error, timeout}
    end.

get_node({Name, Node}) when is_atom(Name), is_atom(Node) ->
    Node;
get_node(Pid) when is_pid(Pid) ->
    node(Pid).

get_dest({Name, Node}) when is_atom(Name), is_atom(Node) ->
    Name;
get_dest(Pid) when is_pid(Pid) ->
    Pid.

name_for_node(Node) ->
  list_to_atom(lists:flatten(io_lib:format("~s_~s", [teleport, Node]))).

term_to_iolist(Term) ->
    [131, term_to_iolist_(Term)].

term_to_iolist_([]) ->
    106;
term_to_iolist_({}) ->
    [104, 0];
term_to_iolist_(T) when is_atom(T) ->
    L = atom_to_list(T),
    Len = length(L),
    %% TODO utf-8 atoms
    case Len > 256 of
        false ->
            [115, Len, L];
        true->
            [100, <<Len:16/integer-big>>, L]
    end;
term_to_iolist_(T) when is_binary(T) ->
    Len = byte_size(T),
    [109, <<Len:32/integer-big>>, T];
term_to_iolist_(T) when is_tuple(T) ->
    Len = tuple_size(T),
    case Len > 255 of
        false ->
            [104, Len, [term_to_iolist_(E) || E <- tuple_to_list(T)]];
        true ->
            [104, <<Len:32/integer-big>>, [term_to_iolist_(E) || E <- tuple_to_list(T)]]
    end;
term_to_iolist_(T) when is_list(T) ->
    %% TODO improper lists
    Len = length(T),
    case Len < 64436 andalso lists:all(fun(E) when is_integer(E), E >= 0, E < 256 ->
                                           true;
                                          (_) -> false
                                       end, T) of
        true ->
            [107, <<Len:16/integer-big>>, T];
        false ->
            [108, <<Len:32/integer-big>>, [[term_to_iolist_(E) || E <- T]], 106]
    end;
term_to_iolist_(T) ->
    %% fallback clause
    <<131, Rest/binary>> = term_to_binary(T),
    Rest.

longorshort() ->
    case split_node(atom_to_list(node()), $@, []) of
        [nonode, nohost] ->
            none;
        [_Name|Tail] ->
            Host = lists:append(Tail),
            case split_node(Host, $., []) of
                [_] ->
                    shortnames;
                L when length(L) > 1 ->
                    longnames
            end
    end.


%%% XXX taken from inet_tcp_dist.erl
%% If Node is illegal terminate the connection setup!!
splitnode(_, none) ->
    {error, nodistribution};
splitnode(Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
        [Name|Tail] when Tail =/= [] ->
            Host = lists:append(Tail),
            case split_node(Host, $., []) of
                [_] when LongOrShortNames =:= longnames ->
                    {error, longnames};
                L when length(L) > 1, LongOrShortNames =:= shortnames ->
                    {error, shortnames};
                _ ->
                    {ok, [Name, Host]}
            end;
        [_] ->
            {error, illegal}
    end.

split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].

ip_and_port(Node) ->
    case splitnode(Node, longorshort()) of
        {ok, [_Name, Host]} ->
            case inet:getaddr(Host, inet) of
                {ok, IP} ->
                    case gen_server:call({teleport_listen_server, Node}, get_port) of
                        {ok, Port} ->
                            {IP, Port};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
