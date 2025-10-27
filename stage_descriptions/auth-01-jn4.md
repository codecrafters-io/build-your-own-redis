In this stage, you'll add support for responding to the `ACL USERS` command.

### The `ACL USERS` command

The [`ACL USERS`](https://redis.io/docs/latest/commands/acl-users/) command is used to list the usernames of all the users currently configured in the Redis' [ACL](https://redis.io/docs/latest/operate/oss_and_stack/management/security/acl/) system.

The `default` user is present in the Redis' ACL system without explicit creation.

Example usage:

```bash
> ACL USERS
1) "default"
2) "john"
3) "lily"
```

It returns a [RESP array](https://redis.io/docs/latest/develop/reference/protocol-spec/#arrays). Each element is a username encoded as a [RESP bulk string](https://redis.io/docs/latest/develop/reference/protocol-spec/#bulk-strings). The usernames are sorted in alphabetical order.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It'll then send send an `ACL USERS` command.

```bash
# Expect RESP array: ["default"]
$ redis-cli ACL USERS
1) "default"
```

The tester will validate that the response is the array `["default"]`, which is RESP Encoded as:

```
*1\r\n
$7\r\n
default\r\n
```

### Notes

- In this stage, you can hardcode the response of `ACL USERS` command to be the array `["default"]`.

- Your server should respond with the `default` user even though no users have been created so far, because Redis always includes a built-in default user in its Access Control List (ACL) system.

- You don't need to implement the `default` user yet. We'll get to that in the later stages.