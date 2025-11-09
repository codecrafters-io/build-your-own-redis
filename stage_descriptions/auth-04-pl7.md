In this stage, you'll add support for responding to the `ACL GETUSER` command with the `passwords` property.

### The `passwords` Property

The `passwords` property lists all hash-encoded passwords associated with a user. 

The `default` user does not have any associated passwords unless explicitly configured:

```bash
> ACL GETUSER default
 1) "flags"
 2) 1) "nopass"
 3) "passwords"
 4) (empty array)
```

Your `ACL GETUSER` response must now include the following:

1. The string `passwords`, encoded as a bulk string.
2. A RESP array containing the list of passwords. Since the `default` user has none, you must hardcode this to be an empty array.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send an `ACL GETUSER` command specifying the `default` user:

```bash
# Expect RESP array: ["flags", ["nopass"], "passwords", []]
$ redis-cli ACL GETUSER default
 1) "flags"
 2) 1) "nopass"
 3) "passwords"
 4) (empty array)
```

The tester will validate that the response is a RESP array with four elements:

1. The first element of the array is the string `flags`, encoded as a RESP bulk string.
2. The second element of the array is a RESP array and contains the `nopass` flag.
3. The third element of the array is the string `passwords`, encoded as a bulk string.
4. The fourth element of the array is an empty array.
