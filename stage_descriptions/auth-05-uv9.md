In this stage, you'll add support for setting the default user's password.

### The `ACL SETUSER` command

The [`ACL SETUSER`](https://redis.io/docs/latest/commands/acl-setuser/) command can be used to modify the properties of an existing user. If this command is used with the `>` rule, it is used to add a password for the given user. Adding a password also clears the `nopass` flag from the user.

Example usage:

```bash
> ACL SETUSER default >mypassword
OK

> ACL GETUSER default
 1) "flags"
 2) 1) "nopass"
 3) "passwords"
 4) 1) "89e01536ac207279409d4de1e5253e01f4a1769e696db0d6062ca9b8f56767c8"
```

The response to the `ACL SETUSER` command is a RESP simple string: `+OK\r\n`.

The password array in the response of `ACL GETUSER` command contains one element and is the SHA-256 hash of the password `mypassword`.

Redis does not store the raw password specified in the `ACL SETUSER` command. Instead, it stores the SHA-256 hash of the password. While validating the password during authentication, the SHA-256 hash of the input password is calculated and matched against the stored list of SHA-256 password hashes. This is done because storing raw passwords is a security vulnerability.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send a `ACL GETUSER` command, specifying the `default` user

```bash
$ redis-cli
# Expect RESP array: ["flags", ["nopass"], "passwords", []]
> ACL GETUSER default
1) "flags"
2) 1) "nopass"
3) "passwords"
4) (empty array)
```

The tester will validate the following for the response of the `ACL GETUSER` command:

- The `nopass` flag is present in the flags array
- The password array is empty

It'll then send a `ACL SETUSER` command, specifying the `default` user and a password.

```bash
# Expect: +OK\r\n
> ACL SETUSER default >mypassword
OK
```

The tester will validate that the response to the `ACL SETUSER` command is `+OK\r\n`.

It'll then send a `ACL GETUSER` command, specifying the `default` user.

```bash
# Expect RESP array: ["flags", ["nopass"], "passwords", ["89e01536ac207279409d4de1e5253e01f4a1769e696db0d6062ca9b8f56767c8"]]
> ACL GETUSER default
1) "flags"
2) 1) "nopass"
3) "passwords"
4) 1) "89e01536ac207279409d4de1e5253e01f4a1769e696db0d6062ca9b8f56767c8"
```

The tester will validate the following for the response of the `ACL GETUSER` command:

- The `nopass` flag is not present in the flags array
- The passwords array contains one element, and the element is the SHA-256 hash of the password, encoded as a RESP bulk string.

### Notes

- Redis uses the SHA-256 hashing algorithm for password storage. You'll need to compute the SHA-256 hash of the provided password and store it.

- The password hash should be stored as a lowercase hexadecimal string.
