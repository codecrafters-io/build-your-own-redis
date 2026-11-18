Your server reports the connection's username. In this stage, you'll add
`ACL GETUSER default` to expose that user's configuration, beginning with an
empty placeholder for its flags.

### The `ACL GETUSER` command

The [`ACL GETUSER`](https://redis.io/docs/latest/commands/acl-getuser/) command
retrieves the properties of a specified user. In Redis, the `default` user is
present from the start and does not need to be created.

For example:

```bash
> ACL GETUSER default
1) "flags"
2) (empty array)
...
```

The return value is a nested RESP array of property name-value pairs for the
user:

```bash
[property_name_1, property_value_1, property_name_2, property_value_2, ...]
```

For this stage, you'll implement just the `flags` property.

### The `flags` property

The `flags` property represents a set of attributes that describe how a user
behaves or what special permissions they have. Each flag is a short label that
defines part of the user’s configuration.

For example, after creating or modifying a user, the flags array might look like
this:

```bash
> ACL GETUSER alice
1) "flags"
2) 1) "on"
   2) "allkeys"
   3) "allcommands"
```

For this stage, return an empty RESP array (`*0\r\n`) as the flags value. This
is a temporary placeholder for your implementation. Redis reports configured
flags, and later stages add `nopass`.

### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send an `ACL GETUSER` command specifying the `default` user:

```bash
# Expect RESP array: ["flags", []]
$ redis-cli
> ACL GETUSER default
1) "flags"
2) (empty array)
```

For this stage, return the two-element RESP array shown above:

1. The first element is the bulk string `flags`.
2. The second element is an empty RESP array.
