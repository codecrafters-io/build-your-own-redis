In this stage, you'll implement the Redis `PING` command.

### What is `PING`?

Redis clients talk to Redis servers by sending **commands** and receiving **responses**.

For example:

```bash
redis> PING                  # client sends a PING command
"PONG"                       # server replies with PONG

redis> SET name Alice        # store the value "Alice" under the key "name"
OK                           # server confirms success
```

`PING` is the simplest Redis command. It’s used to check whether a Redis server is alive and responding.

When a Redis server replies to `PING`, the raw response it sends over the network is actually `+PONG\r\n`, which is the string `PONG` encoded using RESP, a format Redis uses to exchange messages between clients and servers.

For this stage, your task is to simply hardcode a `+PONG\r\n` response, regardless of the incoming command.

You’ll learn more about RESP and how input command parsing works in future stages.
