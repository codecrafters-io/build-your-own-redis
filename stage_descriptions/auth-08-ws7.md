In this stage, you'll add support for authenticating a user using the `AUTH` command.

### The `AUTH` command

The `AUTH` command is used to authenticate the current connection using a username and password pair. A user can be authenticated under following circumstances:

1. The `"on"` flag is present for the user.
2. At least one password should exist for the user, or the `"nopass"` flag should be enabled for the user.

We will get to implementing the `nopass` flag in later stages. For this stage, you can assume that the user will be enabled and will have a password associated with them.

Example usage:

```bash
> AUTH john >wrongpassword
(error) WRONGPASS invalid username-password pair or user is disabled.

> AUTH john >johnspassword
OK
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then create a new user with a password and the `on` flag, and attempt to authenticate using the `AUTH` command.

```bash
$ redis-cli
# Create a user
> ACL SETUSER foo on
# Expect: +OK\r\n
OK

# Set the user's password
> ACL SETUSER foo >mypassword
# Expect: +OK\r\n
OK

# Usage of wrong password
> AUTH foo wrong_password
# Expect error WRONGPASS
(error) WRONGPASS invalid username-password pair or user is disabled.

# Usage of correct password
> AUTH foo mypassword
# Expect +OK\r\n
OK

# The user has no permissions to execute any permissions by default
> ACL WHOAMI
# Expect: NOPERM error
(error) NOPERM User ctrl has no permissions to run the 'acl|whoami' command

# In another client (Because it is an unauthenticated connection)
> ACL WHOAMI
# Expect RESP bulk string: "default" 
"default"
```

The tester will validate that:
- The response to `AUTH foo wrong_password` is an error that starts with `WRONGPASS`.
- The response to `AUTH foo mypassword` is `+OK\r\n`.
- After authentication, when the client tries to execute `ACL WHOAMI`, it receives an authentication error. It is because we haven't granted the user permission to run any commands. We'll get to implementing this in the later stages.

### Notes

- The tester will be lenient in checking the error in case of authentication failure. Any message starting with `WRONGPASS` will be valid. Some valid error messages are:
    - `WRONGPASS wrong password`
    - `WRONGPASS invalid authentication`

- The tester will be lenient in checking the error in case of invalid permissions. Any message starting with `NOPERM` will be valid. Some valid error messages are:
    - `NOPERM insufficient permission`
    - `NOPERM user does not have permission to run this command`

- After authentication, the authenticated user (`foo` in this example) does not have permission to execute any commands (because the user was created with `-@all` by default, which denies all commands).

- You should leave an unauthenticated connection to have every permission since we haven't implemented the default user. We'll get to implementing the default user and its permissions in the later stages.