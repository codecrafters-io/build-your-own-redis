In this stage, you'll add support for setting a password on the `default` user, and verify that new connections require authentication when the `default` user has a password.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then set a password on the `default` user and verify that new connections must authenticate.

```bash
# Connection 1
$ redis-cli
> ACL SETUSER default >mypassword
OK
> ACL GETUSER default
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) 1) "89e01536ac207279409d4de1e5253e01f4a1769e696db0d6062ca9b8f56767c8"
 5) "commands"
 6) "+@all"

# Connection 2 (new connection)
$ redis-cli
> ACL WHOAMI
(error) NOAUTH Authentication required.
> AUTH default mypassword
OK
> ACL WHOAMI
"default"
```

The tester will validate that:
- After setting a password on the `default` user, the `nopass` flag is removed from the flags array.
- New connections cannot execute commands without authentication.
- The `AUTH default mypassword` command successfully authenticates the connection.
- After authentication, `ACL WHOAMI` returns `"default"`.

### Notes

- In connection 2, authentication is required because after default user's password has been set, it'll clear the nopass flag. Thus, new connections cannot be automatically authenticated as the `default` user.