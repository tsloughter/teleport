-module(teleport_conn).

-behaviour(gen_statem).

-export([start_link/3,
         done/1]).

-export([init/1,
         callback_mode/0,
         connected/3,
         disconnected/3,
         enqueued/3,
         dequeued/3,
         code_change/4,
         terminate/3]).

-record(data, {monitor,
               ref,
               conn,
               server,
               broker}).

start_link(Broker, IP, Port) ->
    gen_statem:start_link(?MODULE, {Broker, IP, Port}, []).

done({_, Pid, Ref}) ->
    gen_statem:cast(Pid, {done, Ref});
done(undefined) ->
    ok.

%% @private
init({Broker, IP, Port}) ->
    erlang:process_flag(trap_exit, true),
    {ok, disconnected, #data{server={IP, Port},
                             broker=Broker}, {next_event, internal, connect}}.


callback_mode() ->
    state_functions.

%% Internal

%% init -> connect -> enqueue
%% states: broken, connected, queued, dequeued
%% state: dequeued, 'EXIT' -> enqueue
%% state: dequeued, 'done' -> enqueue
%% state: enqueued, break -> disconnect -> connect -> enqueue

disconnected(EventType, _, Data=#data{server={IP, Port},
                                      broker=Broker}) when EventType =:= internal
                                                       ; EventType =:= timeout ->
    try gen_tcp:connect(IP, Port, [binary, {packet, 4}, {active, once}]) of
        {ok, Conn} ->
            erlang:link(Conn),
            sbroker:async_ask_r(Broker, {self(), Conn}, Conn),
            lager:info("at=connect_succeed conn=~p", [Conn]),
            {next_state, enqueued, Data#data{conn=Conn}};
        Error ->
            lager:warning("at=connect_failure reason=~p", [Error]),
            {next_state, disconnected, #data{broker=Broker}}
    catch
        throw:Reason ->
            lager:warning("at=connect_failure reason=~p", [Reason]),
            {next_state, disconnected, #data{broker=Broker}}
    end;
disconnected(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

connected(internal, {enqueue, Conn}, Data=#data{broker=Broker}) ->
    lager:debug("at=enqueue conn=~p", [Conn]),
    erlang:link(Conn),
    sbroker:async_ask_r(Broker, {self(), Conn}, Conn),
    {next_state, enqueued, Data#data{conn=Conn}};
connected(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

enqueued(info, {_, {go, Ref, Pid, _, _}}, Data) ->
    lager:debug("at=dequeueing ref=~p pid=~p", [Ref, Pid]),
    Mon = erlang:monitor(process, Pid),
    {next_state, dequeued, Data#data{ref = Ref, monitor = Mon}};
enqueued(info, {Pid, {drop, _}}, Data) ->
    lager:debug("at=dropped pid=~p", [Pid]),
    {next_state, connected, Data, {next_event, internal, {enqueue, Pid}}};
enqueued(info, {'EXIT', Pid, _}, Data=#data{broker=Broker}) ->
    lager:warning("at=EXIT pid=~p", [Pid]),
    case cancel_or_await(Broker, Pid) of
        cancelled ->
            {next_state, disconnected, Data#data{ref=undefined, monitor=undefined}, {next_event, internal, connect}};
        {go, _Ref, _Pid, _, _} ->
            %% ??
            {next_state, disconnected, Data#data{ref=undefined, monitor=undefined}, {next_event, internal, connect}};
        {drop, _} ->
            %% ??
            {next_state, disconnected, Data#data{ref=undefined, monitor=undefined}, {next_event, internal, connect}}
    end;
enqueued(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

dequeued(_, {done, Ref}, Data=#data{monitor=Monitor,
                                    broker= Broker,
                                    conn=Conn}) ->
    lager:debug("at=done ref=~p", [Ref]),
    erlang:demonitor(Monitor, [flush]),
    erlang:link(Conn),
    sbroker:async_ask_r(Broker, {self(), Conn}, Conn),
    {next_state, enqueued, Data};
dequeued(info, {'DOWN', _Mon, process, Pid, _}, Data=#data{conn=Conn}) ->
    lager:warning("at=DOWN pid=~p", [Pid]),
    {next_state, connected, Data#data{ref=undefined, monitor=undefined}, {next_event, internal, {enqueue, Conn}}};
dequeued(info, {'EXIT', Pid, _}, Data=#data{conn=Pid}) ->
    %% in dequeued state so don't need to cancel in the broker
    lager:warning("at=EXIT pid=~p", [Pid]),
    {next_state, disconnected, Data#data{ref=undefined, monitor=undefined}, {next_event, internal, connect}};
dequeued(EventType, EventContent, Data) ->
    handle_event(EventType, EventContent, Data).

cancel_or_await(Broker, Tag) ->
    case sbroker:cancel(Broker, Tag, 5000) of
        false ->
            sbroker:await(Tag, 0);
        _ ->
            cancelled
    end.

handle_event(_, _Msg, Data) ->
    {keep_state, Data, []}.

%% @private
code_change(_OldVsn, _State, Data, _Extra) ->
    {keep_state, Data, []}.

%% @private
terminate(_, _, _) ->
    ok.
