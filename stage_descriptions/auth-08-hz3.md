In this stage, you'll add support for setting a user's password using the `ACL SETUSER` command with the password rule.

### Setting user password

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
# Create a user 'foo' with password 'foospassword'
# Expect: +OK\r\n
> ACL SETUSER foo >foospassword
OK

# Expect RESP array: 
# ["flags", ["off"], "passwords", ["88c032bf637c58e7c5446b254aa30cb63bffd2a8ea1983920ec72997872441c1"]]
> ACL GETUSER foo
 1) "flags"
 2) 1) "off"
 3) "passwords"
 4) 1) "88c032bf637c58e7c5446b254aa30cb63bffd2a8ea1983920ec72997872441c1"

# Create the user 'bar' with no password
# Expect: +OK\r\n
> ACL SETUSER bar
OK

# Expect RESP array:
# ["flags", ["off"], "passwords", []]
> ACL GETUSER bar
 1) "flags"
 2) 1) "off"
 3) "passwords"
 4) (empty array)

# Set the password of the user 'bar' to 'barspassword'
# Expect: +OK\r\n
> ACL SETUSER bar >barspassword
OK

# Expect RESP array:
# ["flags", "off", "passwords", ["29286e1ef54a85f9ef040c6e6dbdc3b8d1597a6fef4995b1b1d3617950a0ae93"], "commands", "-@all"]
> ACL GETUSER bar
 1) "flags"
 2) 1) "off"
 3) "passwords"
 4) 1) "29286e1ef54a85f9ef040c6e6dbdc3b8d1597a6fef4995b1b1d3617950a0ae93"
```

The tester will validate the following for the response for the `ACL GETUSER` command:

- The first element of the response array is the literal string `flags`, encoded as a RESP bulk string.
- The second element of the response array is an array containing the flag `off`.
- The third element of the response array is the literal string `passwords`, encoded as a RESP bulk string.
- The fourth element of the response array is an empty array before setting the password.
- After setting the user password, whether at the time of user creation, or later, the fourth element should be an array with one element. This element should be the SHA-256 hash of the user's password encoded as a RESP bulk string.


### Notes

- Redis uses the SHA-256 hashing algorithm for password storage. You'll need to compute the SHA-256 hash of the provided password and store it.

- The password hash should be stored as a lowercase hexadecimal string.

- A user can have multiple passwords. The `passwords` field should be an array of hashes. However, we will only deal  with one password per user.