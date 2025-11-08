In this stage, you'll add support for the `ACL GETUSER` command.

### The `ACL GETUSER` command

The [`ACL GETUSER`](https://redis.io/docs/latest/commands/acl-getuser/) command retrieves the properties of a specified user. In Redis, the `default` user is present from the start and does not need to be created.

For example:
```bash
> ACL GETUSER default
1) "flags"
2) (empty array)
...
```

The command expects the response to be a nested RESP array of property name-value pairs for a user:

```bash
[property_name_1, property_value_1, property_name_2, property_value_2, ...]
```

For this stage, you'll implement just the `flags` property. 

### The `flags` Property

The `flags` property represents a set of attributes that describe how a user behaves or what special permissions they have. Each flag is a short label that defines part of the user’s configuration.

For example, after creating or modifying a user, the flags array might look like this:

```bash
> ACL GETUSER alice
1) "flags"
2) 1) "on"
   2) "allkeys"
   3) "allcommands"
```

For this stage, since the `default` user has no flags to report yet, you will hardcode this to be an empty RESP array (`[]`). 

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

The tester will verify that the response is a RESP array with two elements:

1. The first element is the bulk string `flags`.
2. The second element is an empty RESP array.
