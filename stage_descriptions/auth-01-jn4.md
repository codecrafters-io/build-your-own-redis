In this stage, you'll add support for responding to the `ACL WHOAMI` command.

### The `ACL WHOAMI` Command

The [`ACL WHOAMI`](https://redis.io/docs/latest/commands/acl-whoami/) command is used to return the username the current connection is authenticated with.

In Redis, every new connection is automatically authenticated using the `default` user. This feature can be turned off, making every new connection unauthenticated at first. We'll get to that in the later stages.

For example

```bash
> ACL WHOAMI
"default"
```

It returns the username of the currently authenticated user, encoded as a [RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send an `ACL WHOAMI` command.

```bash
# Expect RESP bulk string: "default"
$ redis-cli
> ACL WHOAMI
"default"
```

Your server should respond with the bulk string `default`:

```
$7\r\n
default\r\n
```

The tester will verify that:
- Your server responds to the `ACL WHOAMI` command
- The response is the string `"default"` encoded as a RESP bulk string
- The RESP encoding is correct (length prefix `$7\r\n` followed by `default\r\n`)

### Notes

- In this stage, you can hardcode the response of the `ACL WHOAMI` command to be `default`. We'll get to enforcing authentication in the later stages.
