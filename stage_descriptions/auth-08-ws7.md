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
# Expect: +OK\r\n
> ACL SETUSER foo on
OK

# Set the user's password
# Expect: +OK\r\n
> ACL SETUSER foo >mypassword
OK

# Usage of wrong password
# Expect error beginning with: WRONGPASS
> AUTH foo wrong_password
(error) WRONGPASS invalid username-password pair or user is disabled.

# Usage of correct password
# Expect: +OK\r\n
> AUTH foo mypassword
OK

# The user has no permissions to execute any permissions by default
# Expect error beginning with: NOPERM
> ACL WHOAMI
(error) NOPERM User ctrl has no permissions to run the 'acl|whoami' command

# In another client (Because it is an unauthenticated connection)
# Expect RESP bulk string: "default" 
> ACL WHOAMI
"default"
```

The tester will validate that:
- The response to `AUTH foo wrong_password` is an error that starts with `WRONGPASS`.
- The response to `AUTH foo mypassword` is `+OK\r\n`.
- After authentication, when the client tries to execute `ACL WHOAMI`, it receives a `NOPERM` error. It is because we haven't granted the user permission to run any commands. We'll get to implementing this in the later stages.

### Notes

- The tester will be lenient in checking error messages: any AUTH failure starting with `WRONGPASS` (e.g., `WRONGPASS wrong password`, `WRONGPASS invalid authentication`) and any permission error starting with `NOPERM` (e.g., `NOPERM insufficient permission`, `NOPERM user does not have permission to run this command`) will be valid.

- After authentication, when the client tries to execute `ACL WHOAMI`, it receives a permission error (starts with `NOPERM`). This is because we haven't granted the user permission to run any commands.

- You should leave an unauthenticated connection to have every permission since we haven't implemented the default user. We'll get to implementing the default user and its permissions in the later stages.