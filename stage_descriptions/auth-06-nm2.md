In this stage, you'll add support for enabling a newly created user using the `ACL SETUSER` command with the `on` flag.

### Enabling a user

The `ACL SETUSER` command can also be used to modify a user's status. The `on` flag can be used with this command to enable the user. If the user does not exist, the specified user will be created and enabled. If the user already exists, the user's status will be set to enabled.

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

It'll then send an `ACL SETUSER` command to create a user. The tester will then send an `ACL SETUSER` command with the `on` flag to enable the user.

```bash
$ redis-cli
# Create the user and enable it using the 'on' flag
# Expect: +OK\r\n
> ACL SETUSER foo on
OK

# Expect RESP array:
# ["flags", ["on"], "passwords", [], "commands", "-@all"]
> ACL 
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "-@all"

# Expect: +OK\r\n
> ACL SETUSER bar
OK

# Expect RESP array:
# ["flags", ["off"], "passwords", [], "commands", "-@all"]
> ACL GETUSER bar
 1) "flags"
 2) 1) "off"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "-@all"

# Expect: +OK\r\n
> ACL SETUSER bar on
OK

# Expect RESP array:
# ["flags", ["on"], "passwords", [], "commands", "-@all"]
> ACL GETUSER bar
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "-@all"
```

The tester will validate the following for the `ACL GETUSER` command:

- The response complies with the response format of the`ACL GETUSER` command.
- The `flags` array in the response contains the flag `off` before enabling the user, and `on` after enabling the user.
- The passwords array contains the SHA-256 hash of the string `foospassword`.
- The command permission rule is `-@all`, meaning that the user does not have permission to run any commands.


### Notes

- The `off` flag can be used to disable a user. However, we will not cover that in our implementation.