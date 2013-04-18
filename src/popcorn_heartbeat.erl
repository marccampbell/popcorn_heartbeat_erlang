-module(popcorn_heartbeat).
-author('mhald@mac.com').
-behaviour(gen_server).

-export([init/1,
         json/4,
         send_health_packet/1,
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

-record(heartbeat_state, {popcorn_host  :: string(),
                popcorn_port  :: number(),
                time          :: number(),
                node          :: string(),
                node_role     :: string(),
                metrics       :: list(),
                node_version  :: string()}).
init(_Params) ->
    Time         = application:get_env(popcorn_heartbeat_erlang, time, 3*1000),
    Node         = application:get_env(popcorn_heartbeat_erlang, node, atom_to_list(node())),
    Role         = application:get_env(popcorn_heartbeat_erlang, node_role, "no_role"),
    Version      = application:get_env(popcorn_heartbeat_erlang, node_version, "no_version"),
    Popcorn_Host = application:get_env(popcorn_heartbeat_erlang, host, "localhost"),
    Popcorn_Port = application:get_env(popcorn_heartbeat_erlang, port, 9125),
    Metrics      = application:get_env(popcorn_heartbeat_erlang, metrics, []),
    io:format("XXX2 metrics ~p~n", [Metrics]),

    io:format("Init params got ~p ~p ~p ~p ~p~n", [Node, Role, Version, Popcorn_Host, Popcorn_Port]),

    erlang:send_after(0, self(), trigger),

    {ok, #heartbeat_state{popcorn_host = Popcorn_Host,
                popcorn_port = Popcorn_Port,
                node         = bin(Node),
                node_role    = bin(Role),
                node_version = bin(Version),
                metrics      = Metrics,
                time         = Time}}.

handle_call({set_time, Time}, _From, State) ->
    {ok, ok, State#heartbeat_state{time=Time}};

handle_call(get_time, _From, State) ->
    {ok, State#heartbeat_state.time, State};

handle_call(_Request, _From, State) ->
    {ok, ok, State}.

handle_cast(_Request, State) ->
    {ok, ok, State}.

handle_event(_Event, State) ->
    {ok, State}.

handle_info(trigger, State) ->
    io:format("Trigger~n"),
    io:format("Time ~p~n", [State#heartbeat_state.time]),
    send_health_packet(State),
    erlang:send_after(State#heartbeat_state.time, self(), trigger),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% send termination packet
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% TODO version number should be read here, or else we don't support upgrades
    {ok, State#heartbeat_state{}}.

send_health_packet(#heartbeat_state{popcorn_host=Host, popcorn_port=Port, node=Node, node_role=Role, node_version=Version, metrics=Metrics} = _State) ->
    Json = json(Role, Node, Version, Metrics),
    io:format("Metrics ~p~n", [Metrics]),
    io:format("Sending up health packet ~p~n", [Json]),
    ibrowse:send_req("http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/api/heartbeat", [], post, Json).

json(Role, Node, Version, Metrics) ->
    Doc = {[
            {<<"heartbeat">>, 
                {[
                    {<<"role">>, Role},
                    {<<"node">>, Node},
                    {<<"node_version">>, Version},
                    {<<"platform">>, <<"erlang">>},
                    {<<"metrics">>, json_metrics(Metrics)}
                ]}
            }]},
    jiffy:encode(Doc).

%% [{name, "memory"}, {type, guage}, {mod, popcorn_heartbeat_util}, {fun, memory_avail}]
json_metrics(Metrics) ->
    Values = [begin
                Mod = proplists:get_value(module, Metric),
                Fun = proplists:get_value(function, Metric),
                Val = get_metric_value(Mod,Fun),
                json_render_metric(Metric, Val)
        end || Metric <- Metrics],
    [Value || Value <- Values, Value =/= undefined].

get_metric_value(undefiend,_) -> undefined;
get_metric_value(_,undefiend) -> undefined;
get_metric_value(Mod,Fun) -> Mod:Fun().

%% {"type":"guage", "name":"load", "value":1.2},
json_render_metric(_, undefined) -> undefined;
json_render_metric(Proplist, Value) ->
    Name = proplists:get_value(name, Proplist),
    Type = proplists:get_value(type, Proplist),
    {[ {<<"name">>, bin(Name)}, {<<"type">>, bin(Type)}, {<<"value">>, Value} ]}.

bin(V) when is_list(V) -> list_to_binary(V);
bin(V) when is_atom(V) -> list_to_binary(atom_to_list(V));
bin(V) when is_binary(V) -> V.

%% HTTP POST popcornserver/api/v1/heartbeat
%% 
%% {
%%   "heartbeat":
%%     {
%%     "role": "xmpp",
%%     "node": "anduin.tigertext.me"
%%     "node_version": "3.1.26"
%%     "platform": "erlang",
%%     "metrics": [
%%         {"type":"guage", "name":"load", "value":1.2},
%%         {"type":"string", "name":"last_user", "value":"mhald"}
%%     ]
%%     }
%% }
%% 
%% possible responses
%% 
%% HTTP status 5xx
%% 
%% {
%%   "error": {
%%      "code": 1,
%%      "description": "json parse failure"
%%   }
%% }
%% 
%% HTTP status 200
%% 
%% {
%%   "missing": ["source_code_mapping"]
%% }
