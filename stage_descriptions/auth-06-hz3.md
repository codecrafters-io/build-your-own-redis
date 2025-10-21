In this stage, you'll add support for setting a user password using the `ACL SETUSER` command with password rules.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `ACL SETUSER` command to create a user, followed by another `ACL SETUSER` command with a password rule.

```bash
$ redis-cli
> ACL SETUSER username >mypassword
OK
> ACL GETUSER username
 1) "flags"
 2) 1) "off"
 3) "passwords"
 4) 1) "89e01536ac207279409d4de1e5253e01f4a1769e696db0d6062ca9b8f56767c8"
 5) "commands"
 6) "-@all"
```

The tester will validate that:
- The response to `ACL SETUSER username >mypassword` is `+OK\r\n`.
- The password is stored as a SHA256 hash in the "passwords" field.

### Notes

- Redis uses SHA256 hashing for password storage. You'll need to compute the SHA256 hash of the provided password and store it.

- The password hash should be stored as a lowercase hexadecimal string.

- A user can have multiple passwords. The "passwords" field should be an array of hashes. However, the tester will only use one password per user.