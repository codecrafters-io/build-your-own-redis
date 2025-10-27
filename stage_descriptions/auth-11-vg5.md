In this stage, you'll add support for setting a user's command permissions to users using the `+@all` ACL rule,

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
# Client 1

# Create a user 'bar' and enable it
# Expect: +OK\r\n
> ACL SETUSER bar on

# Expect: +OK\r\n
> ACL SETUSER bar >barspassword
OK

# Expect: +OK\r\n
> AUTH bar barspassword
OK

# Client 1 is now authenticated as 'bar'
# The user has no permissions to execute any permissions by default
# Expect error beginning with: NOPERM
> ACL WHOAMI
(error) NOPERM User bar has no permissions to run the 'acl|whoami' command

# Client 2 (unauthenticated)

# Expect RESP array:
# ["flags", ["on"], "passwords", ["29286e1ef54a85f9ef040c6e6dbdc3b8d1597a6fef4995b1b1d3617950a0ae93"], "commands", "-@all"]
> ACL GETUSER bar
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) 1) "29286e1ef54a85f9ef040c6e6dbdc3b8d1597a6fef4995b1b1d3617950a0ae93"
 5) "commands"
 6) "-@all"

# Expect: +OK\r\n
> ACL SETUSER bar +@all
OK

# Expect RESP array:
# ["flags", ["on"], "passwords", ["29286e1ef54a85f9ef040c6e6dbdc3b8d1597a6fef4995b1b1d3617950a0ae93"], "commands", "-@all"]
> ACL GETUSER bar
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) 1) "29286e1ef54a85f9ef040c6e6dbdc3b8d1597a6fef4995b1b1d3617950a0ae93"
 5) "commands"
 6) "+@all"


# Client 1 (authenticated as 'bar')

# Expect RESP bulk string: "bar"
> ACL WHOAMI
"bar"

# Expect: +OK\r\n
> SET key value
OK

# Expect RESP bulk string: "value"
> GET key
"value"
```


### Notes

- You should leave an unauthenticated connection to have every permission since we haven't implemented the authentication mechanism for the default user. We'll get to this in the later stages.

- You can always assume that the user specified in the `ACL GETUSER` exists.

- The tester will not send `ACL GETUSER` command with the `default` user. We'll get that in the later stages.
