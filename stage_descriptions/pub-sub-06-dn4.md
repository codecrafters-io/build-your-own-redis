In this stage, you will add support for delivering published messages to subscribed clients.

### Delivering messages

When a client runs `PUBLISH` the command, the message is "delivered" to all clients currently subscribed to the channel.

For example, if a client was subscribed to `channel_1` and the message `hello` was sent, it would see something like this:

```bash
$ redis-cli
> SUBSCRIBE channel_1
1) "subscribe"
2) "channel_1"
3) (integer) 1

# (client is now in subscribed mode)

# When a message is published, it receives the following array: 
1) "message"
2) "channel_1"
3) "hello"
```

Each subscribed client receives an array with 3 elements:

1. "message"(as a RESP bulk string)
1. The channel name (as a RESP bulk string)
1. The message contents (as a RESP bulk string)

In this case, each subscribed client will receive  `["message", "channel_1", "hello"]`, encoded as:

```
*3\r\n
$7\r\n
message\r\n
$9\r\n
channel_1\r\n
$5\r\n
hello\r\n
```

### Tests

The tester will  run your program like this:

```bash
$ ./your_program.sh
```

It will then spawn multiple clients that listen on multiple channels.

```bash
# Client 1 subscribes to foo
$ redis-cli
> SUBSCRIBE foo

# Client 2 subscribes to foo
$ redis-cli
> SUBSCRIBE foo

# Client 3 subscribes to bar
$ redis-cli
> SUBSCRIBE bar
```

The tester will then spawn a separate client and send `PUBLISH` commands for random channels.

```bash
$ redis-cli
> PUBLISH foo "hello"
2
> PUBLISH bar "world"
1
```

It'll verify the `PUBLISH` commands returns the number of clients subscribed to the channel.

For every client, it'll also assert that the client receives the message if it is subscribed. The tester will verify that each subscribed client receives an array like this: `["message", "foo", "hello"]`. When RESP-encoded, this looks like:

```
*3\r\n
$7\r\n
message\r\n
$3\r\n
foo\r\n
$5\r\n
hello\r\n
```

If a client isn't subscribed to a channel, the tester will validate that it doesn't receive messages sent to that channel.
