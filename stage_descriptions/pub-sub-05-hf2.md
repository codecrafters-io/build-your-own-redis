Your server knows which clients subscribe to each channel. Message delivery
comes later. In this stage, you'll add `PUBLISH` and report how many subscribers
a message would reach.

### The `PUBLISH` command

[The PUBLISH command](https://redis.io/docs/latest/commands/publish/) delivers a
message to all clients subscribed to a channel.

```
> PUBLISH channel_name message_contents
(integer) 3
```

The response to the command is the number of clients that are subscribed to the
channel.

Count each client subscribed to that channel once. A channel with no subscribers
produces the RESP integer `:0\r\n`.

### Tests

The tester will run your program like this:

```bash
$ ./your_program.sh
```

It will then spawn multiple clients that listen on multiple channels.

```bash
# Client 1 subscribes to channel "foo"
$ redis-cli
> SUBSCRIBE foo

# Client 2 subscribes to channel "bar"
$ redis-cli
> SUBSCRIBE bar

# Client 3 subscribes to channel "bar"
$ redis-cli
> SUBSCRIBE bar
```

The tester will then publish messages to different channels using `PUBLISH` and
check whether the response matches the number of clients subscribed to the
channel.

```
$ redis-cli PUBLISH bar "msg"
(integer) 2
```

In the above example, the expected response is 2 (encoded as a RESP integer)
since 2 clients are subscribed to channel bar.

Similarly for the `foo` channel

```
$ redis-cli PUBLISH foo "msg"
(integer) 1
```

The tester expects the response to be 1 (encoded as a RESP integer) since only 1
client is subscribed to it.
