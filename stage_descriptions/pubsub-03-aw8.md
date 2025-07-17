In this stage, you'll add support for marking a client as having entered Subscribed mode.

### Subscribe Mode

After a subscribe command is sent, a client enters "Subscribed mode". In this mode, only a subset of commands is allowed:

- `SUBSCRIBE`
- `UNSUBSCRIBE`
- `PSUBSCRIBE`
- `PUNSUBSCRIBE`
- `PING`
- `QUIT`

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

The tester will then send a series of commands, which might be allowed or un-allowed for subscribed mode.

For un-allowed commands (like `SET`, `GET`, and `ECHO`) the tester will verify that your server responds with the following error:

```
> SET key value
- ERR Can't execute 'set': only (P|S)SUBSCRIBE / (P|S)UNSUBSCRIBE / PING / QUIT / RESET are allowed in this context 
```

The tester only verifies that error message starts with "Can't execute '<command_name>'", so you're free to use a flexible error message and not stick to the exact format that Redis uses.

For the `SUBSCRIBE` command, the tester will verify that the response is its usual response.
```bash
> SUBSCRIBE bar
# Expecting ["subscribe", "bar", 2] as RESP-encoded array
```

### Notes

- For un-allowed commands, the tester is lenient in checking error messages so you don't have to stick to the exact format Redis uses. The exact format it checks for is `Can't execute '<command>'` (case-insensitive). Examples of error message strings that will pass tests: 
    - `Can't execute 'set' in subscribed mode`
    - `can't execute 'SET' when one or more subscriptions exist`

- In subscribed mode, `PING` has a different response (it doesn't respond with `+PONG\r\n`). We'll get to this in later stages. 