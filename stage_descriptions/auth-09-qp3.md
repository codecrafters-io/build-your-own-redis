In this stage, you'll add support for extending the `ACL GETUSER` to retrieve user's command permissions.

### Retrieving user command permissions

The `ACL GETUSER` command can also be used to retrieve the command permissions of the specified user.

Example usage:

```bash
# Create a new user 'john'
> ACL SETUSER john
OK

# Retrieve john's information
> ACL GETUSER john
 1) "flags"
 2) 1) "off"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "-@all"
```

Since we haven't implemented command permission setting yet, the commands field will be `-@all` in this stage. The `-@all` string signifies that a newly created user has no permissions to execute any commands. We'll get to implementing command permission setting in the next stage.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `ACL SETUSER` command to create a user, followed by an `ACL GETUSER` command to retrieve the user's information.

```bash
$ redis-cli
# Create a user 'foo'
# Expect: +OK\r\n
> ACL SETUSER foo
OK

# Expect RESP array:
# ["flags", ["off"], "passwords", [], "commands", "-@all"]
> ACL GETUSER foo
 1) "flags"
 2) 1) "off"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "-@all"
```

The tester will validate the following:

- The first element of the response array is the literal string `flags`, encoded as a RESP bulk string.
- The second element of the response array is an array containing the flag `off`.
- The third element of the response array is the literal string `passwords`, encoded as a RESP bulk string.
- The fourth element of the response array is an empty array.
- The fifth element of the response array is the literal string `commands`, encoded as a RESP bulk string.
- The sixth element is the string `-@all`, encoded as a RESP bulk string.

### Notes

- You can always assume that the user specified in the `ACL GETUSER` exists.

- The tester will not send `ACL GETUSER` command with the `default` user. We'll get that in the later stages.


