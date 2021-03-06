-module(popcorn_heartbeat_erlang).
-author('mhald@mac.com').
-vsn('0.1').

-behaviour(application).

-export([start/0, stop/0]).
-export([start/2, stop/1]).

start() -> 
    application:start(?MODULE).

start(_StartType, _StartArgs) ->
    popcorn_heartbeat_sup:start_link().

stop() ->
    application:stop(?MODULE).

stop(_State) ->
    ok.
