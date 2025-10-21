In this stage, you'll add support for creating users that can authenticate without a password using the `nopass` flag.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then create a user with the `nopass` flag and verify that the user can authenticate without providing a password.

```bash
$ redis-cli
> ACL SETUSER foo on nopass
OK

> ACL GETUSER foo
 1) "flags"
 2) 1) "on"
    2) "nopass"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "-@all"

> AUTH foo "random-password"
OK

> ACL WHOAMI
(error) NOAUTH Authentication required.
```

The tester will also verify that setting a password on a user clears the `nopass` flag:

```bash
$ redis-cli
> ACL SETUSER bar on nopass
OK
> ACL SETUSER bar >mypassword
OK
> ACL GETUSER bar
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) 1) "89e01536ac207279409d4de1e5253e01f4a1769e696db0d6062ca9b8f56767c8"
 5) "commands"
 6) "-@all"
> AUTH bar
(error) WRONGPASS invalid username-password pair or user is disabled.
> AUTH bar mypassword
OK
```

The tester will validate that:
- The `ACL GETUSER foo` response includes `"nopass"` in the flags array.
- The `AUTH foo` command (without a password argument) succeeds.
- After authentication, the user still cannot execute commands due to lack of permissions.
- When a password is set on user `bar`, the `nopass` flag is removed from the flags array.
- After the `nopass` flag is cleared, `AUTH bar` (without password) fails.
- Authentication with the password succeeds.

### Notes
