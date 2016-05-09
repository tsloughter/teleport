-module(teleport_client).

-behavior(shackle_client).

-export([init/0,
         setup/2,
         handle_request/2,
         handle_data/2,
         terminate/1]).

-record(state, {}).

init() ->
    {ok, #state{}}.

setup(_Socket, State) ->
    {ok, State}.

handle_request({send, Dest, Msg}, State) ->
    {ok, 0, teleport:term_to_iolist({send, Dest, Msg}), State}.

handle_data(_, State) ->
    {ok, [], State}.

terminate(_State) ->
    ok.
