-module(popcorn_heartbeat_sup).
-author('mhald@mac.com').

-behaviour(supervisor).

-export([start_link/0, init/1]).


-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {{one_for_one, 5, 60}, [supervisor:child_spec()]}}.
init([]) ->
  {ok, {{one_for_one, 5, 60}, [
	   {popcorn_heartbeat, {popcorn_heartbeat, start_link, []}, permanent, 2000, worker, [popcorn_heartbeat]}
  ]}}.
