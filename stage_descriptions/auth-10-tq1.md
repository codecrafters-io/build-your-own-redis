In this stage, you'll add support for creating users that can authenticate without a password using the `nopass` flag.

### The `nopass` flag

The `nopass` is one of the user flags in Redis that makes the user so that every password will work against this user. It also removes all the passwords for the user if their passwords list is non-empty.

Example usage:

```bash
> ACL SETUSER newuser on
OK

> ACL SETUSER newuser nopass

# When 'nopass' flag is enabled, it is possible to authenticate as this user using any password
> AUTH newuser random_string
OK
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then create a user using the `ACL SETUSER` command specifying a password. The tester will then send a `ACL SETUSER` command  with the `nopass` flag.  and verify that the user can authenticate without providing a password.

```bash
$ redis-cli
# Create a user and enable it
> ACL SETUSER foo on
# Expect: +OK\r\n
OK

# Add a password for the user
> ACL SETUSER foo >foospassword
# Expect: +OK\r\n
OK

# Provide all permissions for the user
> ACL SETUSER foo +@all
# Expect: +OK\r\n
OK


> ACL GETUSER foo
# Expect RESP array
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) 1) "88c032bf637c58e7c5446b254aa30cb63bffd2a8ea1983920ec72997872441c1"
 5) "commands"
 6) "+@all"

> ACL SETUSER foo nopass
# Expect +OK\r\n
OK

> ACL GETUSER foo
# Expect RESP array:
 1) "flags"
 2) 1) "on"
    2) "nopass"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "+@all"

> AUTH foo "random-password"
# Expect: +OK\r\n
OK

> ACL WHOAMI
# Expect: "foo"
"foo"
```

The tester will validate the following for the `ACL GETUSER` command:

- The response complies with the response format of the`ACL GETUSER` command.
- The `flags` array in the response contains the flag`on`.
- The `flags` array in the response does not conain the flag `nopass` before the `ACL SETUSER foo nopass` command is sent.
- The `flags` array in the response contains the flag `nopass` after the `ACL SETUSER foo nopass` command is sent.
- The passwords array contains the SHA-256 hash of the string `foospassword` before the `ACL SETUSER foo nopass` command is sent.
- The passwords array is empty after the `ACL SETUSER foo nopass` command is sent.
- The command permission rule is `+@all`, meaning that the user has permissions to run any commands.
