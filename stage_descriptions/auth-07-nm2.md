In this stage, you'll add support for enforcing authentication for the `default` user.

### Enforcing `default` user authentication

By default, every new connection is automatically authenticated as the `default` user. This is because the `nopass` flag is set for the `default` user from the start. However, after clearing this flag (by setting a password for the `default` user), new connections are not automatically authenticated as the `default` user. Connections which have already been authenticated will remain authenticated.

Example usage:

```bash
# Client 1
$ redis-cli
> ACL SETUSER default >password
OK

# This connection remains authenticated as the default user
> ACL WHOAMI
"default"

# Client 2
# This connection is not authenticated
> ACL WHOAMI
(error) NOAUTH Authentication required.
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send commands to two different clients.

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

The tester will validate the following:

1. The first client (Client 1) is still authenticated as the `default` user.

2. A second client (Client 2) receives a `NOAUTH` error when attempting to execute commands without authentication.
    - This is because after we set the password for the `default` user, the `nopass` flag is also cleared. This disables auto-authentication of new users as the `default` user.