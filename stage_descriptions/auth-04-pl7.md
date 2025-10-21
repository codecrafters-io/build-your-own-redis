In this stage, you'll add support for creating a new user using the `ACL SETUSER` command.

### Creating a user with `ACL SETUSER` command

The `ACL SETUSER` command can be used to create a new user in the Redis' ACL system.

Example usage:

```bash
# Using `ACL SETUSER` to create a new user
> ACL SETUSER john
OK

# List the existing users
> ACL USERS
1) "default"
2) "john"
```

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

Then, it will send multiple `ACL SETUSER` commands specifying a different username each time. For example, the tester may send the following commands:
 
```bash
$ redis-cli
> ACL SETUSER foo
OK
> ACL SETUSER bar
OK
```

The tester will then send an `ACL USERS` command.
```
> ACL USERS
1) "bar"
2) "default"
3) "foo"
```

In this case, the tester will validate that the response is the array `["bar", "default", "foo"]`, which is RESP Encoded as:

```
$3\r\n
$3\r\n
bar\r\n
$7\r\n
default\r\n
$3\r\n
foo\r\n
```

### Notes

- The usernames in the array must be sorted in an alphabetical order.

- Even though no user with username `default` was created, your server must respond as if a user with the username `default` exists.

- You don't need to implement the default user. We'll get to that in the later stages.