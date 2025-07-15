In this stage, you'll add support for subscribing to multiple channels using the `SUBSCRIBE` command.

### Multiple SUBSCRIBE commands

A client can send `SUBSCRIBE` multiple times to subscribe to different channels. For example:

```bash
$ redis-cli
> SUBSCRIBE channel_1
1) "subscribe"
2) "channel_1"
3) (integer) 1

(subscribed mode)> SUBSCRIBE channel_2
1) "subscribe"
2) "channel_2"
3) (integer) 2
```

A client can also subscribe to the same channel multiple times, but the total subscribed channels count (returned as the 3rd item in the response array) won't change.

For example:

```bash
$ redis-cli
> SUBSCRIBE channel_1
1) "subscribe"
2) "channel_1"
3) (integer) 1

# The count remains 1 since channel is same
(subscribed mode)> SUBSCRIBE channel_1
1) "subscribe"
2) "channel_1"
3) (integer) 1
```

The subscribed channels count is per-client, i.e. how many channels the current client is subscribed to.

### Tests

The tester will  run your program like this:

```bash
$ ./your_program.sh
```

It will then spawn a client and send multiple `SUBSCRIBE` commands specifying a new channel each time.

```bash
$ redis-cli
> SUBSCRIBE foo
# Expect ["subscribe", "foo", 1]

> SUBSCRIBE bar
# Expect ["subscribe", "bar", 2]

> SUBSCRIBE bar
# Expect ["subscribe", "bar", 2]
```

The tester will repeat this with other clients and verify that the subscribed channel counts are maintained per-client.
