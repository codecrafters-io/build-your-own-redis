In this stage, you'll add support for enforcing authentication for the `default` user.

### Enforcing `default` User Authentication

When you create a new connection, it is automatically authenticated as the `default` user. This happens because the `nopass` flag is set for the `default` user from the start.

Once you set a password for the `default` user, new connections will no longer be automatically authenticated. However, any connections that were already authenticated will stay logged in.

For example:

```bash
# Client 1
$ redis-cli ACL SETUSER default >password
OK

# This connection remains authenticated as the default user
> ACL WHOAMI
"default"

# Client 2
# This connection is not authenticated
$ redis-cli ACL WHOAMI
(error) NOAUTH Authentication required.
```

When an unauthenticated connection tries to execute a command, return the simple error: `NOAUTH Authentication required.`

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send commands to two different clients:

```bash
# Client 1
$ redis-cli
# Expect: +OK\r\n
> ACL SETUSER default >newpassword
OK

# Expect RESP bulk string: "default"
> ACL WHOAMI
"default"

# Client 2 (new connection)
$ redis-cli
# Expect error starting with: "NOAUTH"
> ACL WHOAMI
(error) NOAUTH Authentication required.
```

The tester will verify that:
- Client 1 can still execute commands and remains authenticated as the `default` user.
- Client 2 receives a `NOAUTH` error when trying to execute commands without authentication.
