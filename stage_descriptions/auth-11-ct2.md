In this stage, you'll add support for retrieving the `default` user's information using the `ACL GETUSER` command.

### The `default` user

The `default` user is a built-in user that exists in every Redis server. Unlike regular users created with `ACL SETUSER`, the `default` user has special default properties:

- It is enabled by default (`on` flag)
- It has the `nopass` flag, allowing automatic authentication without providing a password
- It has permission to execute all commands (`+@all`)

When a client connects to Redis without explicitly authenticating (`AUTH`), it is automatically authenticated as the `default` user. This allows commands to be executed immediately without requiring explicit authentication. However, if the `"nopass"` flag is cleared from the default user (by setting a password for the `default` user), new connections cannot automatically be authenticated as the `default` user and `AUTH` command must be used for authentication.

Example usage:

```bash
# Query the default user's properties
> ACL GETUSER default
 1) "flags"
 2) 1) "on"
    2) "nopass"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "+@all"

# Any new connection can execute commands without authentication
> ACL WHOAMI
"default"

> SET mykey myvalue
OK
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `ACL GETUSER` command to retrieve the `default` user's information.

```bash
$ redis-cli
> ACL GETUSER default
 1) "flags"
 2) 1) "on"
    2) "nopass"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "+@all"
```

The tester will validate the following for the response of the `ACL GETUSER default` command:

- The response complies with the response format of the`ACL GETUSER` command.
- The `flags` array contains the flag `on` and `nopass`.
- The passwords array is an empty array.
- The command permission rule for the user is `+@all`, meaning that the user has permissions to run every command.

### Notes

- Unlike regular users created with `ACL SETUSER` (which start with `off`, no passwords, and `-@all`), the `default` user starts with `on`, `nopass`, and `+@all`.

- The `nopass` flag in the response indicates that the user can authenticate without providing a password. This is why new connections can execute commands immediately.