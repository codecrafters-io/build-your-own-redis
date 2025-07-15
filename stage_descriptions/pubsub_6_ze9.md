In this stage, you'll add support for the `UNSUBSCRIBE` command, which is used to unsubscribe from a channel.

### The `UNSUBSCRIBE` Command

This command removes the client from one or more channels. Example usage:

```bash
$ redis-cli
> SUBSCRIBE foo
1) "subscribe"
2) "foo"
3) (integer) 1
(subscribed mode)> SUBSCRIBE bar
1) "subscribe"
2) "bar"
3) (integer) 2
(subscribed mode)> UNSUBSCRIBE foo
1) "unsubscribe"
2) "foo"
3) (integer) 1
(subscribed mode)> UNSUBSCRIBE bar
1) "unsubscribe"
2) "bar"
3) (integer) 0
```

When unsubscribed, the server replies with a RESP Array with 3 elements:

1. "unsubscribe" (as a RESP bulk string)
1. The channel name (as a RESP bulk string)
1. Count of remaining channels the client has subscribed to (as a RESP integer)

When unsubscribing from a channel that has not been subscribed yet, it returns the array with no change in the remaining channels count. For eg,

```bash
$ redis-cli
> subscribe foo
1) "subscribe"
2) "foo"
3) (integer) 1

(subscribed mode)> unsubscribe bar
1) "unsubscribe"
2) "bar"
3) (integer) 1
```

### Tests

The tester will  run your program like this:

```bash
$ ./your_program.sh
```

It will then spawn multiple clients that listen on multiple channels:

```bash
# Client 1 subscribes to 'foo' and 'baz'
$ redis-cli
> SUBSCRIBE foo
> SUBSCRIBE baz

# Client 2 subscribes to 'foo' and 'bar'
$ redis-cli
> SUBSCRIBE foo
> SUBSCRIBE bar
```

The tester will then spawn a separate client that publishes messages to random channels.

```bash
$ redis-cli
> PUBLISH foo "before-unsubscribe"
```

It will verify that any clients subscribed to the channel receive the message, and that other clients don't.

The tester will then issue random `UNSUBSCRIBE` commands to the subscribed clients:

```bash
# In client 1, which was subscribed to 'foo' and 'baz'
> UNSUBSCRIBE foo
# Expects ["unsubscribed", "foo", 1] as an RESP array
```

Finally, the tester will again publish messages to random channels:.

```bash
> PUBLISH foo "after-unsubscribe"
```

It will verify that any clients subscribed to the channel receive the message, and that other clients don't.