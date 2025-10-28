In this stage, you'll add support for responding to the `ACL GETUSER` command with the `nopass` flag set.

### The `nopass` flag

The `nopass` flag is one of the user flags in Redis.

- If the `nopass` flag is set for a user, the authentication succeed with an arbitrary password for the user.
- Setting the `nopass` flag clears the associated passwords for the given user.
- The default user has the `nopass` flag set. Due to this, new connections are automatically authenticated as the `default` user. (This behavior can be changed, and we'll get to this in the later stages.)

Example usage:
```bash
> ACL GETUSER default
1) "flags"
2) 1) "nopass"
```

The flags are encoded as a RESP bulk string. We'll get to enforcing the behavior of `nopass` flag in the later stages.

In this stage, you only need to respond to the `ACL GETUSER` command with the `nopass` flag set.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `ACL GETUSER` command specifying the `default` user.

```bash
# Expect RESP array: ["flags", ["nopass"]]
$ redis-cli
> ACL GETUSER default
1) "flags"
2) 1) "nopass"
```

The tester will validate the following for the response:

1. The first element of the array is the string `flags`, encoded as a RESP bulk string.
2. The second element of the array is a RESP array, and contains the `nopass` flag.