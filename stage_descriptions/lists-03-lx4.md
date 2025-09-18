In this stage, you'll add support for appending multiple elements in a single `RPUSH` command.

### `RPUSH` with multiple elements

The `RPUSH` command supports adding multiple elements to a list at once. This works whether you're creating a new list or appending elements to an existing one.

```bash
# Creating a new list with multiple elements
> RPUSH another_list "bar" "baz"
(integer) 2

# Appending multiple elements to an existing list
> RPUSH another_list "foo" "bar" "baz"
(integer) 5
```

The response to each command is the new length of the list returned as a [RESP integer](https://redis.io/docs/latest/develop/reference/protocol-spec/#integers).

### Tests

The tester will execute your program like this:

```
./your_program.sh
```

It will then send multiple `RPUSH` commands, each including more than one element to append to the list.

```bash
$ redis-cli RPUSH list_key "element1" "element2" "element3"
# Expect: (integer) 3 → encoded as :3\r\n

$ redis-cli RPUSH list_key "element4" "element5"
# Expect: (integer) 5 → encoded as :5\r\n
```

In each case, the tester will expect the response to be the length of the list encoded as a RESP integer.

### Notes

- `RPUSH` accepts multiple elements even when creating a new list, not only when appending to an existing list. 
