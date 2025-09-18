In this stage, you'll add support for creating a new list using the `RPUSH` command.

### The `RPUSH` Command

The [`RPUSH`](https://redis.io/docs/latest/commands/rpush/) command is used to append elements to a list. If the list doesn't exist, it is created first.

Example usage:

```bash
# Creating a new list with a single element
> RPUSH list_key "foo"
(integer) 1

# Appending a single element to an existing list
> RPUSH list_key "bar"
(integer) 2
```

The return value is the number of elements in the list after appending. This number is encoded as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send the following command to your program:

```bash
$ redis-cli RPUSH list_key "element"
```

The tester will verify that the response to the command is `:1\r\n`, which is 1 (the number of elements in the list), encoded as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

### Notes

- In this stage, you'll only need to handle creating a new list with a single element. We'll get to handling `RPUSH` for existing lists and multiple elements in later stages. 
