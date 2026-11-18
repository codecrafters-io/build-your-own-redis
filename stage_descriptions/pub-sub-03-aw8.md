Your server records subscriptions per client. In this stage, you'll put
subscribed connections into a restricted mode while leaving other clients'
command handling unchanged.

### Subscribed mode

With RESP2, the protocol used in this challenge, `SUBSCRIBE` puts the connection
in subscribed mode. Redis limits the commands allowed on that connection. These
include:

- `SUBSCRIBE`
- `UNSUBSCRIBE`
- `PSUBSCRIBE`
- `PUNSUBSCRIBE`
- `PING`
- `QUIT`

This stage checks that further `SUBSCRIBE` commands work and ordinary commands
such as `SET`, `GET`, and `ECHO` are rejected. You don't need to implement
pattern subscriptions or other new commands from the list yet.

The server will reject any other commands with an error, example:

```
$ redis-cli
> SUBSCRIBE channel
1) "subscribe"
2) "channel"
3) (integer) 1

(subscribed mode)> ECHO hey
(error) ERR Can't execute 'echo': only (P|S)SUBSCRIBE / (P|S)UNSUBSCRIBE / PING / QUIT / RESET are allowed in this context
```

### Tests

The tester will run your program like this:

```bash
$ ./your_program.sh
```

It will then send a `SUBSCRIBE` command to your server to enter Subscribed mode:

```bash
$ redis-cli
> SUBSCRIBE foo
# Expecting ["subscribe", "foo", 1]
```

The tester will then send a series of commands, which might be allowed or
un-allowed for subscribed mode.

For un-allowed commands (like `SET`, `GET`, and `ECHO`) the tester will verify
that your server responds with the following error:

```
> SET key value
-ERR Can't execute 'set': only (P|S)SUBSCRIBE / (P|S)UNSUBSCRIBE / PING / QUIT / RESET are allowed in this context\r\n
```

Send a RESP simple error with the message prefix
`ERR Can't execute '<command_name>'`. Use uppercase `ERR` followed by a space.
The `can't execute '<command_name>'` text is checked case-insensitively. The
rest of the message may vary.

For the `SUBSCRIBE` command, the tester will verify that the response is its
usual response.

```bash
> SUBSCRIBE bar
# Expecting ["subscribe", "bar", 2] as RESP-encoded array
```

### Notes

- Examples of accepted error message text are:

  - `ERR Can't execute 'set' in subscribed mode`
  - `ERR can't execute 'SET' when one or more subscriptions exist`

- In subscribed mode, `PING` has a different response. Later stages cover it.
