Your user configuration exposes `nopass`. In this stage, you'll add an empty
`passwords` property, ready to expose password hashes when they are configured.

### The `passwords` property

The `passwords` property contains password hashes. A hash is a fixed-size value
computed from a password's bytes. Later stages will compute these hashes when a
password is added.

The `default` user does not have any associated passwords unless explicitly
configured:

```bash
> ACL GETUSER default
 1) "flags"
 2) 1) "nopass"
 3) "passwords"
 4) (empty array)
```

Your `ACL GETUSER` response must now include the following:

1. The string `passwords`, encoded as a bulk string.
2. A RESP array containing the password hashes. No passwords have been
   configured yet, so return an empty array.

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

Your response should include these four elements in order:

1. The first element of the array is the string `flags`, encoded as a RESP bulk
   string.
2. The second element of the array is a RESP array and contains the `nopass`
   flag.
3. The third element of the array is the string `passwords`, encoded as a bulk
   string.
4. The fourth element of the array is an empty array.
