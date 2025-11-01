In this stage, you'll add support for setting the user's flag using the `ACL SETUSER` command.

### Enabling a user

The `ACL SETUSER` command can also be used to modify a user's status. The `on` flag can be used with this command to enable the user. If the user does not exist and the 'on' flag is provided, the specified user will be created and enabled, otherwise it will be created disabled. If the user already exists and the 'on' flag is provided, the user's status will be set to enabled.

Authenticating using a user is possible only if the user is enabled.

Example usage:

```bash
# Create a new user 'john' which is enabled upon creation
> ACL SETUSER john on
OK

# Create a new user 'lily' (The user is not enabled by default)
> ACL SETUSER lily
OK

# Enable the user 'lily'
> ACL SETUSER lily on
OK
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll send an ACL SETUSER command with the 'on' flag to create and enable a user. The tester will then send an `ACL SETUSER` command with the `on` flag to enable the user.

```bash
$ redis-cli
# Create the user and enable it using the 'on' flag
# Expect: +OK\r\n
> ACL SETUSER foo on
OK

# Expect RESP array:
# ["flags", ["on"]]
> ACL 
 1) "flags"
 2) 1) "on"

# Expect: +OK\r\n
> ACL SETUSER bar
OK

# Expect RESP array:
# ["flags", ["off"]]
> ACL GETUSER bar
 1) "flags"
 2) 1) "off"

# Expect: +OK\r\n
> ACL SETUSER bar on
OK

# Expect RESP array:
# ["flags", ["on"]]
> ACL GETUSER bar
 1) "flags"
 2) 1) "on"
```

The tester will validate the following for the `ACL GETUSER` command:

- The first element of the response array is the literal string `flags`, encoded as a RESP bulk string.
- The second element of the response array is also an array.
- The first element of the nested array is `off` before enabling the user, and `on` after enabling the user.

### Notes

- The `off` flag can be used to disable a user. However, we will not cover that in our implementation.

- You can always assume that the user specified in the `ACL GETUSER` exists.

- The tester will not send `ACL GETUSER` command for the `default` user. We'll get that in the later stages.
