In this stage, you'll add support for responding to the `ACL GETUSER` command with the `nopass` flag set.

### The `nopass` flag

The `nopass` flag is one of the user flags in Redis. It controls the password authentication behaviour:

- If `nopass` is set for a user, authentication succeeds with any password (or no password)
- Setting `nopass` clears any passwords associated with the user

The `default` user has `nopass` set by default, which is why new connections are automatically authenticated.

For example:
```bash
> ACL GETUSER default
1) "flags"
2) 1) "nopass"
...
```

The flags are encoded as a RESP array of bulk strings. 

For this stage, you only need to respond to the `ACL GETUSER` command with the `nopass` flag set. We'll get to enforcing the behavior of the `nopass` flag in later stages.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send an `ACL GETUSER` command specifying the `default` user.

```bash
# Expect RESP array: ["flags", ["nopass"]]
$ redis-cli
> ACL GETUSER default
1) "flags"
2) 1) "nopass"
```

The tester will verify that the response is a RESP array with two elements:
1. The first element is the bulk string `flags`.
2. The second element is a RESP array containing one element: the bulk string `nopass`.
