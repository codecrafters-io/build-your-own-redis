In this stage, you'll add support for creating a new user using the `ACL SETUSER` command.

### Creating a user

The `ACL SETUSER` command can be used to create a new user in the Redis' ACL system. If the user already exists, flags or rules are used to modify the user's properties.

Example usage:

```bash
# Using `ACL SETUSER` to create a new user 'eric'
> ACL SETUSER eric
OK

# Using `ACL SETUSER` to create a new user 'layla'
> ACL SETUSER layla
OK

# Since 'eric' is already existing user and no flags/rules are specified, no changes should be made on the user
> ACL SETUSER eric
OK

# List the existing users
> ACL USERS
1) "default"
2) "john"
```

If the user already exists, the `ACL SETUSER username` command without any options or flags does not change anything for the user.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

Then, it will send multiple `ACL SETUSER` commands specifying a username each time. For example, the tester may send the following commands:
 
```bash
$ redis-cli
# Expect: +OK\r\n
> ACL SETUSER foo
OK

# Expect: +OK\r\n
> ACL SETUSER bar
OK

# Expect: +OK\r\n
> ACL SETUSER foo
OK
```

The tester will then send an `ACL USERS` command.

```bash
# Expect: ["bar", "default", "foo"]
> ACL USERS
1) "bar"
2) "default"
3) "foo"
```

In this case, the tester will validate that the response is the array `["bar", "default", "foo"]`.

### Notes

- The usernames in the array must be sorted in an alphabetical order.

- Even though no user with the username `default` exists, your server must respond as if a user with the username `default` exists.

- You don't need to implement the default user. We'll get to that in the later stages.