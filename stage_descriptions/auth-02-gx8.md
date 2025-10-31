In this stage, you'll add support for responding to the `ACL GETUSER` command.

### The `ACL GETUSER` command

The [`ACL GETUSER`](https://redis.io/docs/latest/commands/acl-getuser/) is used to retrieve the properties the specified user. In Redis, the `default` user is present from the start, without having to create it explicitly.

The `ACL GETUSER` returns multiple properties of the user, among which `flags` is one. In this stage, you'll add support for responding to the `ACL GETUSER` command with only the flags property.

Example usage:
```bash
> ACL GETUSER default
1) "flags"
2) (empty array)
```

The second element of the resposne is the flags array. This is because a user can have multiple flags associated with it. In this stage, you can hardcode the flags array to be an empty array.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `ACL GETUSER` command specifying the `default` user.

```bash
# Expect RESP array: ["flags", []]
$ redis-cli
> ACL GETUSER default
1) "flags"
2) (empty array)
```

The tester will validate the following for the response:

1. The first element of the array is the string `flags`, encoded as a RESP bulk string.
2. The second element of the array is a RESP array.

### Notes

- A user can have multiple flags. This is why the value of flags property is an array.

- The second element of the array is the flags array, which contains the user flags. We'll get to this in the later stages.