In this stage, you’ll add support for the  `SUBSCRIBE` command.

### The `SUBSCRIBE` Command

[The `SUBSCRIBE` command](https://redis.io/docs/latest/commands/subscribe/) registers the client as a subscriber to a channel.

Example Usage:

```bash
$ redis-cli
> SUBSCRIBE mychan
1) "subscribe"
2) "mychan"
3) (integer) 1
Reading messages... (press Ctrl-C to quit or any key to type command)
```

The response is a RESP Array which contains three elements:

1. "subscribe" (as a RESP bulk string)
1. The channel name (as a RESP bulk string) 
1. The number of channels this client has subscribed to so far (as a RESP integer)

### Tests

The tester will  run your program like this:

```bash
$ ./your_program.sh
```

It will then send a `SUBSCRIBE` command with a channel name.

```
$ redis-cli SUBSCRIBE foo
```

The tester will then expect the response to be `["subscribe", "foo", 1]`, which is encoded in RESP as:

```
*3\r\n
$9\r\n
subscribe\r\n
$3\r\n
foo\r\n
:1\r\n
```

### Notes

- In this stage, you'll only need to handle a `SUBSCRIBE` command being sent once and from a single client. We'll get to handling multiple `SUBSCRIBE` commands in later stages.