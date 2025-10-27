In this stage, you'll add support for granting command permissions to users using the `+@all` ACL rule, and you'll verify that authenticated users can execute commands they have permission for.

### The `+@all` rule

The `ACL SETUSER` can be used to grant a user to run every command using the `+@all` rule.

Example usage:

```bash
# In the first client
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

# In the second client (using the default user)
> ACL SETUSER john +@all

# In the first client
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
# Expect: +OK\r\n
> ACL SETUSER foo on
OK

# Expect: +OK\r\n
> ACL SETUSER foo >mypassword
OK

# Expect: +OK\r\n
> AUTH foo mypassword
OK

# Expect error beginning with: NOPERM
> ACL WHOAMI
(error) NOPERM User ctrl has no permissions to run the 'acl|whoami' command

## Client 2 (unauthenticated => should have all permissions)
# Expect RESP array:
# ["flags", ["on"], "passwords", ["88c032bf637c58e7c5446b254aa30cb63bffd2a8ea1983920ec72997872441c1"], "commands", "-@all"]
> ACL GETUSER foo
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) 1) "88c032bf637c58e7c5446b254aa30cb63bffd2a8ea1983920ec72997872441c1"
 5) "commands"
 6) "-@all"

$ redis-cli
# Expect: +OK\r\n
> ACL SETUSER foo +@all
OK

# Expect RESP array:
# ["flags", ["on"], "passwords", ["88c032bf637c58e7c5446b254aa30cb63bffd2a8ea1983920ec72997872441c1"], "commands", "+@all"]
> ACL GETUSER foo
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) 1) "88c032bf637c58e7c5446b254aa30cb63bffd2a8ea1983920ec72997872441c1"
 5) "commands"
 6) "+@all"

## Client 1 (authenticated as foo)
# Expect RESP bulk string: "foo"
> ACL WHOAMI
"foo"

# Expect: +OK\r\n
> SET key 34
OK

# Expect RESP bulk string: "34"
> GET key
"34"

## Client 2 (unauthenticated connection)
# Expect RESP bulk string: "default"
> ACL WHOAMI
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