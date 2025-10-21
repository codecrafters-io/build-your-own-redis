In this stage, you'll add support for enabling a newly created user using the `ACL SETUSER` command with the `on` flag.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `ACL SETUSER` command to create a user, followed by another `ACL SETUSER` command with the `on` flag to enable the user.

```bash
$ redis-cli
> ACL SETUSER username on >password
OK
> ACL GETUSER username
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) 1) "89e01536ac207279409d4de1e5253e01f4a1769e696db0d6062ca9b8f56767c8"
 5) "commands"
 6) "-@all"
```

The tester will validate that the "flags" field in the response contains "on".

### Notes

- The `on` flag enables a user account.

- When a user is enabled, the "flags" field should contain "on" instead of "off".

- You can also use the `off` flag to disable a user account.