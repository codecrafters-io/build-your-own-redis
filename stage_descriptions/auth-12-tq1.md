In this stage, you'll add support for creating users that can authenticate without a password using the `nopass` flag.

### The `nopass` flag

The `nopass` flag is one of the user flags in Redis that allows any password to work for this user. It also removes all passwords for the user if their password list is non-empty.

Example usage:

```bash
> ACL SETUSER newuser on
OK

> ACL SETUSER newuser nopass

# When 'nopass' flag is enabled, it is possible to authenticate as this user using any password
> AUTH newuser random_string
OK

> ACL WHOAMI
"newuser"
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then create a user using the `ACL SETUSER` command, specifying a password. The tester will then send an `ACL SETUSER` command with the `nopass` flag and verify that the user can authenticate using any password.

```bash
$ redis-cli
# Create a user and enable it
# Expect: +OK\r\n
> ACL SETUSER foo on
OK

# Add a password for the user
# Expect: +OK\r\n
> ACL SETUSER foo >foospassword
OK

# Provide all permissions for the user
# Expect: +OK\r\n
> ACL SETUSER foo +@all
OK


# Expect RESP array:
# ["flags", ["on"], "passwords", ["88c032bf637c58e7c5446b254aa30cb63bffd2a8ea1983920ec72997872441c1"], "commands", "+@all"]
> ACL GETUSER foo
 1) "flags"
 2) 1) "on"
 3) "passwords"
 4) 1) "88c032bf637c58e7c5446b254aa30cb63bffd2a8ea1983920ec72997872441c1"
 5) "commands"
 6) "+@all"

# Expect: +OK\r\n
> ACL SETUSER foo nopass
OK

# Expect RESP array:
# ["flags", ["on", "nopass"], "passwords", [], "commands", "+@all"]
> ACL GETUSER foo
 1) "flags"
 2) 1) "on"
    2) "nopass"
 3) "passwords"
 4) (empty array)
 5) "commands"
 6) "+@all"

# Expect: +OK\r\n
> AUTH foo "random-password"
OK

# Expect RESP bulk string: "foo"
> ACL WHOAMI
"foo"
```

The tester will validate the following for the responses:

- In the response of `ACL GETUSER` command:
   - The `nopass` flag is not present before the flag has been set for the user.
   - The `nopass` flag is present after the flag has been set for the user.
   - The `passwords` array should contain the SHA-256 hash of the user's password before the `nopass` flag has been set.
   - The `passwords` array should be an empty array after `nopass` flag has been set.

- The response to the `AUTH` with random password should be `+OK\r\n` after `nopass` flag has been set for the user.

- The response to `ACL WHOAMI` should be the username of the user, encoded as a RESP bulk string.