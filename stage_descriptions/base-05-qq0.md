So far, every response has been fixed. In this stage, you'll implement
[ECHO](https://redis.io/commands/echo) so the response depends on the client's
argument.

### The `ECHO` command

The `ECHO` command is used for testing and debugging, similar to `PING`.

```bash
$ redis-cli PING # The command you implemented in the previous stages
PONG
$ redis-cli ECHO hey # The command you'll implement in this stage
hey
```

It accepts a single argument and sends it back as a
[RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

A bulk string has the form `$<length>\r\n<data>\r\n`, where `<length>` is the
number of bytes in `<data>`. For example, `$3\r\nhey\r\n` carries the three
bytes of `hey`. The angle-bracketed names describe fields to replace, and aren't
sent literally.

Commands arrive as RESP arrays of bulk strings. An array begins with
`*<count>\r\n`, followed by that many encoded values. For `ECHO hey`, the count
is `2`: the command name and its argument. Parse the argument and return it as a
bulk string.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `ECHO` command with an argument to your server:

```bash
$ redis-cli ECHO hey
```

The exact bytes you'll receive will be a
[RESP Array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays)
that looks something like `*2\r\n$4\r\nECHO\r\n$3\r\nhey\r\n`. This is the RESP
encoding of `["ECHO", "hey"]`.

The tester will expect to receive `$3\r\nhey\r\n` as a response. That's the
string `hey` encoded as a
[bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

### Notes

- A reusable RESP parser can also handle the commands introduced in later
  stages. TCP delivers a byte stream, so a read can contain part of a command or
  several commands. RESP's counts and lengths identify the boundaries.
- Redis command names are case-insensitive, so `ECHO`, `echo`, and `EcHo` are
  all valid commands.
- The tester will send a random string as an argument to the `ECHO` command, so
  you won't be able to hardcode the response to pass this stage.
