This extension adds password authentication for Redis's `default` user. In this
stage, you'll implement `ACL WHOAMI` to report the user associated with a
connection.

### The `ACL WHOAMI` command

ACL stands for access control list. Redis's ACL commands inspect and configure
users and their access. This extension implements a subset for the built-in
`default` user.
[`ACL WHOAMI`](https://redis.io/docs/latest/commands/acl-whoami/) reports the
current connection's username.

By default, every new connection in Redis is automatically authenticated as the
`default` user.

For example:

```bash
> ACL WHOAMI
"default"
```

The command returns the username of the currently authenticated user, encoded as
a
[RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

This default authentication behavior can be changed so that new connections
start out unauthenticated. We'll cover that in later stages.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send an `ACL WHOAMI` command:

```bash
# Expect RESP bulk string: "default"
$ redis-cli ACL WHOAMI
"default"
```

The tester will expect to receive `$7\r\ndefault\r\n` as a response. That's the
string `default` encoded as a
[bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

### Notes

- In this stage, you can hardcode the response of the `ACL WHOAMI` command to be
  `default`. We'll get to enforcing authentication in the later stages.
