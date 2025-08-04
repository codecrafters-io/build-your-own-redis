In this stage, you'll add support for responding to `PING` when a client is in subscribed mode.

### `PING` in subscribed mode

If a ping command is issued from a client after it enters subscribed mode, Redis does not respond with the usual response (`+PONG\r\n`). Instead it responds with a RESP array of two elements:

1. "pong" (encoded as a bulk string)
2. "" (empty bulk string)

### Tests

The tester will  run your program like this:

```bash
$ ./your_program.sh
```

It will then send a `SUBSCRIBE` command to your server to enter Subscribed mode:

```bash
$ redis-cli
> SUBSCRIBE foo
# Expecting ["subscribe", "foo", 1]
```

The tester will then send a `PING` command.
```
> PING 
```

It will expect the response to be an RESP-encoded array `["pong", ""]`, which would look like this:
```
*2\r\n
$4\r\n
pong\r\n
$0\r\n
\r\n
```

The tester will also send a `PING` command using a separate client, which is not subscribed to any channels.
```
> PING
```
It will expect the response to be `+PONG\r\n`, which is "PONG" encoded as a RESP simple string.