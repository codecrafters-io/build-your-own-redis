In this stage, you'll add support for enforcing authentication for the `default` user.

### Enforcing `default` user authentication

In Redis, every new connection is automatically authenticated as the `default` user.

The `default` user can be modified just like any other user using the `ACL SETUSER` command. When you add a password to the `default` user, it changes the authentication behavior for all new connections:

- Adding a password with `>password` automatically removes the `nopass` flag
- New connections are no longer automatically authenticated
- Clients must use the `AUTH` command to authenticate before executing commands

This is useful for securing a Redis instance by requiring authentication for all connections.

Example usage:

```bash
# Initially, the default user has no password and the nopass flag
> ACL GETUSER default
 1) "flags"
 2) 1) "on"
    2) "nopass"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "+@all"

# Set a password on the default user
> ACL SETUSER default >mypassword
OK

# The nopass flag is now removed
> ACL GETUSER default
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) 1) "89e01536ac207279409d4de1e5253e01f4a1769e696db0d6062ca9b8f56767c8"
 5) "commands"
 6) "+@all"

# New connections must now authenticate
# (In a new connection)
> ACL WHOAMI
(error) NOAUTH Authentication required.

> AUTH default mypassword
OK

> ACL WHOAMI
"default"

```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then set a password on the `default` user and verify that new connections must authenticate.

```bash
# Client 1
$ redis-cli

# Expect: +OK\r\n
> ACL SETUSER default >mypassword
OK

# Expect RESP bulk string: "default"
> ACL WHOAMI
"default"

# Expect RESP array:
# ["flags", ["on"], "passwords", ["89e01536ac207279409d4de1e5253e01f4a1769e696db0d6062ca9b8f56767c8"], "commands", "+@all"]
> ACL GETUSER default
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) 1) "89e01536ac207279409d4de1e5253e01f4a1769e696db0d6062ca9b8f56767c8"
 5) "commands"
 6) "+@all"

# Client 2 (new connection)
$ redis-cli

# Expect error starting with: NOAUTH
> ACL WHOAMI
(error) NOAUTH Authentication required.

# Expect: +OK\r\n
> AUTH default mypassword
OK

# Expect RESP bulk string: "default"
> ACL WHOAMI
"default"
```

The tester will validate that:
- After setting a password on the `default` user:
    - The `nopass` flag is removed from the flags array.
    - The SHA-256 hash of the password is listed in the passwords array.

- The `AUTH` command returns `+OK\r\n`. (The correct password will be provided for the user.)

- New connections cannot execute commands without authentication, and will fail with `NOAUTH` error.



### Notes

- The tester is lenient in checking the error message in case of an unauthenticated connection. Any error message that begins with `NOAUTH` is valid. For example, the error messages the tester will accept are:
    - `NOAUTH Authentication required`
    - `NOAUTH Not authenticated`

- Existing connections that were already authenticated remain authenticated. Only new connections are affected by the password change.