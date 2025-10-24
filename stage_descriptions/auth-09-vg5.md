In this stage, you'll add support for granting command permissions to users using the `+@all` ACL rule, and you'll verify that authenticated users can execute commands they have permission for.

### The `+@all` rule

The `ACL SETUSER` can be used to grant a user to run every command using the `+@all` rule.

Example usage:

```bash
# Create a user john and enable it
> ACL SETUSER john on
OK

# Set john's password
> ACL SETUSER john >johnspassword

# Authenticate using the user 'john'
> AUTH john johnspassword
OK

# Run 'ACL WHOAMI' using the user 'john'
> ACL WHOAMI
(error) NOPERM User john has no permissions to run the 'acl|whoami' command

# In another client (using the default user)
> ACL SETUSER john +@all

# Run 'ACL WHOAMI' after granting permission to run all commands
> ACL WHOAMI
"john"
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then perform the following sequence:

1. Create a user with a password and enable them.
2. Authenticate as that user on one connection
3. Verify that the authenticated user cannot execute commands due to insufficient permissions.
4. Grant the user permission to execute all commands using a second connection (unauthenticated connection)
5. Verify that the authenticated user can now execute commands

For example, the tester may send the following commands

```bash
## Client 1
$ redis-cli
> ACL SETUSER foo on
# Expect: +OK\r\n
OK

> ACL SETUSER foo >mypassword
# Expect: +OK\r\n
OK

> AUTH foo mypassword
# Expect: +OK\r\n
OK

> ACL WHOAMI
# Expect: NOAUTH error
(error) NOPERM User ctrl has no permissions to run the 'acl|whoami' command

## Client 2 (unauthenticated => should have all permissions)
> ACL GETUSER foo
# Expect RESP array:
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) 1) "88c032bf637c58e7c5446b254aa30cb63bffd2a8ea1983920ec72997872441c1"
 5) "commands"
 6) "-@all"

$ redis-cli
> ACL SETUSER foo +@all
OK

> ACL GETUSER foo
# Expect RESP array:
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) 1) "88c032bf637c58e7c5446b254aa30cb63bffd2a8ea1983920ec72997872441c1"
 5) "commands"
 6) "+@all"

## Client 1 (authenticated as foo)
> ACL WHOAMI
# Expect bulk string: "foo"
"foo"
> SET key value
# Expect: +OK\r\n
OK
> GET key
# Expect bulk string: "value"
"34"

## Client 2 (unauthenticated connection)
> ACL WHOAMI
# Expect bulk string: "default"
"default"
```

The tester will validate following for the `ACL GETUSER` command: 

- The response is a valid response to the `ACL GETUSER` command
- The `flags` array in the response contains the flag `on`.
- The passwords array contains the SHA-256 hash of the string `foospassword`.
- The command permission rule is `-@all` before granting the permission and `+@all` after granting the permission.

- The `ACL WHOAMI` command returns the username of the authenticated connection.

- The unauthenticated connection can execute commands without authentication.
- The unauthenticated connection returns `ACL WHOAMI` with the RESP bulk string `default`.

### Notes

- You should leave an unauthenticated connection to have every permission since we haven't implemented the default user. We'll get to implementing the default user and its permissions in the later stages.