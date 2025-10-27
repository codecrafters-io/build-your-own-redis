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

It'll then do the following:

1. In the first client:
    1. Create a new user, enable it, and set its password
    2. Authenticate as the user using the correct password
    3. Try to run `ACL WHOAMI` command

2. In a second client, which is unauthenticated:
    1. It'll use `ACL GETUSER` to retrieve the new user's properties.
    2. Grant the new user the permission to run all commands using the `+@all` rule.
    3. It'll use the `ACL GETUSER` to retrieve the new user's properties.

3. In the first client:
    - Run the `ACL WHOAMI` command

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
```

The tester will verify the following for the responses:

- For the `ACL GETUSER` command:
    - The command permission is `-@all` before granting the user permissions to run commands.
    - The command permission is `+@all` after the user has been granteed the permission to run all commands.

- For the `ACL WHOAMI` command:
    - The response should be an error starting with `NOPERM` if the user does not have permission to run the command.
    - The response should be the username, encoded as a RESP bulk string, after the user has been granted the permission to run the command.

- For the `AUTH` command:
    - The response should be `+OK\r\n`.


### Notes

- You should leave an unauthenticated connection to have every permission since we haven't implemented the authentication mechanism for the default user. We'll get to this in the later stages.

- You can always assume that the user specified in the `ACL GETUSER` exists.

- The tester will not send `ACL GETUSER` command for the `default` user. We'll get that in the later stages.
