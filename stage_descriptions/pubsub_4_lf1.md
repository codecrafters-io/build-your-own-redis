In this stage, you'll add support for modifying the response of `PING` command in the subscribed mode.

### Subscribe Mode (Modification of `PING`)

If a ping command is issued from a client after it enters the subscribed mode, the server does not respond with `+PONG\r\n`, which is the usual response of `PING` command. Instead, the response is a RESP-Array of two elements

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

It will expect the response to be an RESP-encoded array `["ping", ""]`, which would look like this:
```
*2\r\n
$4\r\n
pong\r\n
$0\r\n
\r\n
```