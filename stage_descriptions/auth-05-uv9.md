Your server reports an empty password list. In this stage, you'll let
`ACL SETUSER` add a password hash for the `default` user and remove its `nopass`
flag.

### The `ACL SETUSER` command

The [`ACL SETUSER`](https://redis.io/docs/latest/commands/acl-setuser/) command
modifies the properties of an existing user.

When the command is used with the `>` rule, it adds a password for the specified
user:

```bash
> ACL SETUSER default >mypassword
OK
```

The server responds with `OK` encoded as a RESP simple string (`+OK\r\n`).

### Password storage

Adding a password for a user with `ACL SETUSER` has two effects:

- The password is stored as a SHA-256 hash.
- The `nopass` flag is automatically removed.

For example:

```bash
> ACL SETUSER default >mypassword
OK

> ACL GETUSER default
 1) "flags"
 2) (empty array)
 3) "passwords"
 4) 1) "89e01536ac207279409d4de1e5253e01f4a1769e696db0d6062ca9b8f56767c8"
```

Notice that the `nopass` flag is now gone from the `flags` array. Also, the
`mypassword` SHA-256 hash is stored as a bulk string in the `passwords` array.

SHA-256 produces a 256-bit value. Redis exposes it as 64 lowercase hexadecimal
characters. Matching this format is required for compatibility. A hash avoids
storing the plaintext password, but someone who obtains the hash can still try
guesses against it.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send an `ACL GETUSER` command, specifying the `default` user:

```bash
# Expect RESP array: ["flags", ["nopass"], "passwords", []]
$ redis-cli ACL GETUSER default
1) "flags"
2) 1) "nopass"
3) "passwords"
4) (empty array)
```

The tester will verify that:

- The `nopass` flag is present in the flags array.
- The `passwords` array is empty.

Next, the tester will send a `ACL SETUSER` command, specifying the `default`
user and a password:

```bash
# Expect: +OK\r\n
> ACL SETUSER default >mypassword
OK
```

Your server must respond with `+OK\r\n`.

Finally, the tester will send a `ACL GETUSER` command, specifying the `default`
user:

```bash
# Expect RESP array: ["flags", [], "passwords", ["89e01536ac207279409d4de1e5253e01f4a1769e696db0d6062ca9b8f56767c8"]]
> ACL GETUSER default
1) "flags"
2) (empty array)
3) "passwords"
4) 1) "89e01536ac207279409d4de1e5253e01f4a1769e696db0d6062ca9b8f56767c8"
```

The tester will validate the following for your response:

- The `nopass` flag is no longer present.
- The `passwords` array contains the SHA-256 hash of `mypassword` encoded as a
  bulk string.
