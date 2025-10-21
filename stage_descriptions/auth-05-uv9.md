In this stage, you'll add support for responding to the `ACL GETUSER` command.

### The `ACL GETUSER` command

The [`ACL GETUSER`](redis.io/docs/latest/commands/acl-getuser/) command is used to list all the ACL rules defined for an existing ACL user. The `ACL GETUSER` command returns following information about the specified user:

1. Flags
2. User passwords
3. Command permissions
4. Keys permissions
5. Channel permissions
6. Selectors

Example Usage:

```
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

1. `"flags"`, encoded as a RESP bulk string.

2. List of flags, encoded as a RESP array. Each flag is encoded as a RESP bulk string. A user can have multiple flags, so this element is an array.

3. `"passwords"`, encoded as a RESP bulk string.

4. List of passwords associated with the user, encoded as a RESP array. Each element is a SHA-256 hash of the original password, encoded as a RESP bulk string. A user can have multiple flags, so this element is a RESP array.

5. `"commands"`, encoded as a RESP bulk string.

6. ACL rules for command permissions, encoded as a RESP bulk string.

The official Redis implementation returns other elements as well, which include key permissions, channel permission, and selectors. You can read more about it [here](https://redis.io/docs/latest/commands/acl-getuser/). In our implementation, we'll only focus on flags, passwords, and command permissions.

### Tests

```bash
$ ./your_program.sh
```

It'll then send an `ACL SETUSER` command to create a user, followed by an `ACL GETUSER` command to retrieve the user's information.

```bash
$ redis-cli
> ACL SETUSER username
OK
> ACL GETUSER username
 1) "flags"
 2) 1) "off"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "-@all"
```

The tester will validate the following:

1. The first element of the response array is the literal string `"flags"`.
2. The second element of the response array is an array of length 1.
3. The first element of this array is `"off"`.
4. The third element of the response array is the literal string `"passwords"`.
5. The fourth element of the response array is an empty array.
6. The fifth element of the response array is the literal string `"commands"`.
7. The sixth element is the string `"-@all"`.


### Notes

- The `"off"` string in the flags array signifies that the user is not enabled by default. In this stage, you can hardcode this value. We'll get to manipulating this flag in the later stages.

- The `"-@all"` string in the commands signifies that a newly created user has permissions to execute no any commands. In this stage, you can hardcode this value. We'll get to implementing command permissions in the later stages.

- The tester will not send `ACL GETUSER` command with users which do not exist.

- The tester will not send `ACL GETUSER` command with the `default` user. We'll get that in the later stages.
