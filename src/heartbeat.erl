-module(heartbeat).
-author('mhald@mac.com').
-behaviour(gen_server).

-export([init/1,
         start_link/0,
         handle_call/3,
         handle_cast/2,
         handle_event/2,
         handle_info/2,
         terminate/2,
         code_change/3
]).

-spec start_link() -> {ok, pid}.
start_link() -> gen_server:start_link(?MODULE, [], []).

-record(state, {popcorn_host  :: string(),
                popcorn_port  :: number(),
                time          :: number(),
                node_role     :: string(),
                node_version  :: string()}).
init(Params) ->
    Time         = proplists:get_value(time, Params, 3*1000),
    Node_Role    = proplists:get_value(node_role, Params, "no_role"),
    Node_Version = proplists:get_value(node_version, Params, "no_version"),
    Popcorn_Host = proplists:get_value(popcorn_host, Params, "localhost"),
    Popcorn_Port = proplists:get_value(popcorn_port, Params, 9125),

    erlang:send_after(Time, self(), trigger),

    {ok, #state{socket       = Socket,
                popcorn_host = Popcorn_Host,
                popcorn_port = Popcorn_Port,
                node_role    = Node_Role,
                node_version = Node_Version,
                time         = Time}}.

handle_call({set_time, Time}, _From, State) ->
    {ok, ok, State#state{time=Time}};

handle_call(get_time, _From, State) ->
    {ok, State#state.time, State};

handle_call(_Request, _From, State) ->
    {ok, ok, State}.

handle_cast(_Request, State) ->
    {ok, ok, State}.

handle_event(_Event, State) ->
    {ok, State}.

handle_info(trigger, State) ->
    send_health_packet(State#state.socket, State#state.popcorn_host, State#state.popcorn_port, State#state.node_version),
    erlang:send_after(State#state.time, self(), trigger),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% send termination packet
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% TODO version number should be read here, or else we don't support upgrades
    {ok, State#state{}}.

send_health_packet(Socket, Popcorn_Host, Popcorn_Port, _Version) ->
    ok.
