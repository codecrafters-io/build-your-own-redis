In this stage, you'll add support for extending the `ACL GETUSER` command to retrieve user passwords.

### Retrieving user passwords

The `ACL GETUSER` command can be used to retrieve the password list of the specified user.

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
```

Since we haven't implemented password setting yet, the passwords array should be empty in this stage. We'll get to implementing password setting in the next stage.

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
# ["flags", ["off"], "passwords", []]
> ACL GETUSER foo
 1) "flags"
 2) 1) "off"
 3) "passwords"
 4) (empty array)
```

The tester will validate the following:

- The first element of the response array is the literal string `flags`, encoded as a RESP bulk string.
- The second element of the response array is an array containing the flag `off`.
- The third element of the response array is the literal string `passwords`, encoded as a RESP bulk string.
- The fourth element of the response array is an empty array. This is because no password has been set for the user.

### Notes

- You can always assume that the user specified in the `ACL GETUSER` exists.

- The tester will not send `ACL GETUSER` command for the `default` user. We'll get that in the later stages.

