-module(popcorn_heartbeat_util).

-export([memory_used/0]).

memory_used() ->
    Memory_Used = [Mem || {_,Mem} <- erlang:system_info(allocated_areas)],
    lists:sum(Memory_Used).
