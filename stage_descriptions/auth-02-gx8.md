In this stage, you'll add support for responding to the `ACL WHOAMI` command.

### The `WHOAMI` command

The [`WHOAMI`](https://redis.io/docs/latest/commands/acl-whoami/) command is used to return the username the current connection is authenticated with.

In Redis, every new connection is automatically authenticated using the `default` user. This feature can be turned off, making every new connection unauthenticated at first. We'll get to that in the later stages.

Example usage:


```bash
> ACL WHOAMI
"default"
```

It returns the username of currently authenticated user, encoded as a [RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings).

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `ACL WHOAMI` command.

```bash
$ redis-cli ACL WHOAMI
# Expect RESP bulk string: "default"
"default"
```

The tester will validate that the response is the string `default`, which is RESP encoded as:

```
$7\r\n
default\r\n
```

### Notes

- In this stage, you can hardcode the response of the `ACL WHOAMI` command to be `default`. We'll get to returning the authenticated user's username in the later stages.

- You don't need to implement the `default` user yet. We'll get to that in the later stages.

- You don't need to authenticate new connection using the `default` user yet. We'll get to that in the later stages.