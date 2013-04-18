# Configuration

Heartbeat is configured in its own section in the app configuration file.

Required fields

- host and port of the popcorn server
- node_role and node_version of the current node

Optional fields

- metrics are a list of named and typed metrics, each with a callback module/function

```erlang
{popcorn_heartbeat_erlang, [
       {host, "127.0.0.1"},
       {port, 9125},
       {node_role,    "xmpp"},
       {node_version, "1.2.3"},
       {metrics, [
           [{name, "memory"}, {type, guage}, {module, popcorn_heartbeat_util}, {function, memory_used}]
       ]}
]}
```

# Testing

```erlang
application:start(popcorn_heartbeat_erlang), timer:sleep(1000), application:stop(popcorn_heartbeat_erlang).
```
