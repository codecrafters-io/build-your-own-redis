In this stage, you'll add support for granting command permissions to users using the `+@all` ACL rule, and you'll verify that authenticated users can execute commands they have permission for.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then perform the following sequence:

1. Create a user with a password and enable them
2. Authenticate as that user on one connection
3. Grant the user permission to execute all commands using a second connection (authenticated as `default`)
4. Verify that the authenticated user can now execute commands

```bash
$ redis-cli
> ACL SETUSER foo on >mypassword
OK
> AUTH foo mypassword
OK
> ACL WHOAMI
(error) NOAUTH Authentication required.

# Connection 2 (Not authenticated yet => Should have all permissions)
$ redis-cli
> ACL SETUSER foo +@all
OK

# Back to Connection 1 (authenticated as foo)
> ACL WHOAMI
"foo"
> SET bar 34
OK
> GET bar
"34"
```

The tester will validate that:
- After granting `+@all` permissions, the user `foo` can execute commands like `ACL WHOAMI`, `SET`, and `GET`.
- The `ACL WHOAMI` command returns the username of the authenticated connection.
- The `default` user connection can execute commands without authentication.

### Notes