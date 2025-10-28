In this stage, you'll add support for responding to the `ACL GETUSER` command with the passwords property.

### The `passwords` property

A user in the Redis ACL system can have one or more passwords associated with them. The `ACL GETUSER` command also returns the `passwords` property of the specified user.

Example usage:
```bash
> ACL GETUSER default
 1) "flags"
 2) 1) "nopass"
 3) "passwords"
 4) (empty array)
```

The fourth element of the response is the passwords array. The default user does not have any associated passwords unless explicitly configured. This is why the passwords array is empty for the default user.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `ACL GETUSER` command specifying the `default` user.

```bash
$ redis-cli
# Expect RESP array: ["flags", ["nopass"], "passwords", []]
> ACL GETUSER default
 1) "flags"
 2) 1) "nopass"
 3) "passwords"
 4) (empty array)
```

The tester will validate the following for the response:

1. The first element of the array is the string `flags`, encoded as a RESP bulk string.
2. The second element of the array is a RESP array and contains the `nopass` flag.
3. The third element of the array is the string `passwords`, encoded as a RESP bulk string.
4. The fourth element of the array is an empty array because no passwords have been specified for the default user.