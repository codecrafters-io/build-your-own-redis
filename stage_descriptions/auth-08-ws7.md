In this stage, you'll add support for authenticating a user using the `AUTH` command, but making sure that newly created users cannot execute commands.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then create a new user with a password and the `on` flag, and attempt to authenticate using the `AUTH` command.

```bash
$ redis-cli

> ACL SETUSER foo on >mypassword
OK

> AUTH foo wrong_password
(error) WRONGPASS invalid username-password pair or user is disabled.

> AUTH foo mypassword
OK

> ACL WHOAMI
(error) NOAUTH Authentication required.

# In another client (Because it is an unauthenticated connection)
$ redis-cli
> ACL WHOAMI
"default"
```

The tester will validate that:
- The response to `AUTH foo wrong_password` is an error that starts with `WRONGPASS`.
- The response to `AUTH foo mypassword` is `+OK\r\n`.
- After authentication, when the client tries to execute `ACL WHOAMI`, it receives an authentication error.

### Notes

- For now, assume that the password is always correct. You don't need to verify the password yet - we'll get to that in later stages.

- After authentication, the authenticated user (`foo` in this example) does not have permission to execute any commands (because the user was created with `-@all` by default, which denies all commands).
