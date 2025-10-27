In this stage, you'll add support for setting a user password using the `ACL SETUSER` command with the password rule.

### Modifying user passwords

The `ACL SETUSER` command can also be used to modify a user's properties. The `>` rule can be used with this command to add a new password for a user. If the user does not exist, the specified user will be created with the specified password. If the user already exists, the password will be added to the list of passwords for that user.

Redis does not store the raw password specified in the `ACL SETUSER` command. Instead, it stores the [SHA-256](https://blog.boot.dev/cryptography/how-sha-2-works-step-by-step-sha-256/) hash of the password. While validating the password during authentication, the SHA-256 hash of the input password is calculated and matched against the stored list of SHA-256 password hashes. This is done because storing raw passwords is a security vulnerability.

Example usage:

```bash
# Create a new user 'john' with the password 'johnspassword'
> ACL SETUSER john >johnspassword
OK

# Create a new user 'lily' (This user has no password by default)
> ACL SETUSER lily
OK

# Add the password 'lilyspassword' to the password list for the user 'lily'
> ACL SETUSER lily >lilyspassword
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send an `ACL SETUSER` command to create a user specifying a password.

```bash
$ redis-cli
# Expect: +OK\r\n
> ACL SETUSER foo >foospassword
OK

# Expect RESP array: 
# ["flags", ["off"], "passwords", ["88c032bf637c58e7c5446b254aa30cb63bffd2a8ea1983920ec72997872441c1"], "commands", "-@all"]
> ACL GETUSER foo
 1) "flags"
 2) 1) "off"
 3) "passwords"
 4) 1) "88c032bf637c58e7c5446b254aa30cb63bffd2a8ea1983920ec72997872441c1"
 5) "commands"
 6) "-@all"
```

The tester will validate the following for the response for the `ACL GETUSER` command:

- The response complies with the response format of the `ACL GETUSER` command.
- The `flags` array contains the flag `off`.
- The SHA-256 hash of the string `foospassword` is present in the passwords array.
- The command permission rule for the user is `-@all`, meaning that the user has no permissions to run any commands.

### Notes

- Redis uses the SHA-256 hashing algorithm for password storage. You'll need to compute the SHA-256 hash of the provided password and store it.

- The password hash should be stored as a lowercase hexadecimal string.

- A user can have multiple passwords. The `passwords` field should be an array of hashes. However, the tester will only use one password per user.