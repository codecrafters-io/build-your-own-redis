In this stage, you'll add support for responding to the `ACL WHOAMI` command.

### The `ACL WHOAMI` Command

The [`ACL WHOAMI`](https://redis.io/docs/latest/commands/acl-whoami/) command returns the username associated with the current connection. 

By default, every new connection in Redis is automatically authenticated as the `default` user. For example:

```bash
> ACL WHOAMI
"default"
```

The command returns the username of the currently authenticated user, encoded as a [RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

This default authentication behavior can be changed so that new connections start out unauthenticated. We'll cover that in later stages.

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

The tester will expect to receive `$7\r\ndefault\r\n` as a response. That's the string `default` encoded as a [bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

### Notes

- In this stage, you can hardcode the response of the `ACL WHOAMI` command to be `default`. We'll get to enforcing authentication in the later stages.
