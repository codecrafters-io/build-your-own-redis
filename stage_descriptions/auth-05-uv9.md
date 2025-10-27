In this stage, you'll add support for retrieving the user flags using the `ACL GETUSER` command.

### The `ACL GETUSER` command
The [`ACL GETUSER`](redis.io/docs/latest/commands/acl-getuser/) command is used to list all the ACL rules defined for an existing ACL user.

Example usage:

```bash
> ACL GETUSER default
 1) "flags"
 2) 1) "on"
    2) "nopass"
    3) "sanitize-payload"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "+@all"
 7) "keys"
 8) "~*"
 9) "channels"
10) "&*"
11) "selectors"
12) (empty array)
```

The `ACL GETUSER` command returns a [RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays). The elements of the response array are:

1. The literal string `flags`, encoded as a RESP bulk string.

2. List of flags, encoded as a RESP array. Each element of the array is encoded as a RESP bulk string. A user can have multiple flags, so this element is an array.

3. The literal string `passwords`, encoded as a RESP bulk string.

4. List of passwords associated with the user, encoded as a RESP array. Each element is a SHA-256 hash of the original password, encoded as a RESP bulk string. A user can have multiple passwords, so this element is also a RESP array.

5. THe literal string `commands`, encoded as a RESP bulk string.

6. ACL rules for command permissions, encoded as a RESP bulk string.

The official Redis implementation returns other elements as well, which include key permissions, channel permission, and selectors. You can read more about it [here](https://redis.io/docs/latest/commands/acl-getuser/). In our implementation, we'll only focus on user flags, passwords, and command permissions.

- The `off` string in the flags array signifies that the user is not enabled by default.

- The string `-@all` in the commands signifies that a newly created user has no permissions to execute any commands.

In this stage, we'll only deal with retrieving the `flags` field of a user.

### Tests

```bash
$ ./your_program.sh
```

It'll then send an `ACL SETUSER` command to create a user, followed by an `ACL GETUSER` command to retrieve the user's information.

```bash
$ redis-cli
# Expect: +OK\r\n
> ACL SETUSER username
OK

# Expect RESP array:
# ["flags", ["off"]]
> ACL GETUSER username
 1) "flags"
 2) 1) "off"
```

The tester will validate the following:

- The first element of the response array is the literal string `flags`, encoded as a RESP bulk string.
- The second element of the response array is also an array.
- The first element of the nested array is `off`, encoded as a RESP bulk string.

### Notes

- You can always assume that the user specified in the `ACL GETUSER` exists.

- The tester will not send `ACL GETUSER` command with the `default` user. We'll get that in the later stages.