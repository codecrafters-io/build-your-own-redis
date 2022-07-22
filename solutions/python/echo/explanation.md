In Stage 5, we explore streams processing.

In previous stages, the incoming client requests were always `PING`, and so we were able to hardcode our responses
as `PONG`. Adding support for `ECHO` means that we first need to determine whether the incoming request is an `ECHO`
or a `PING`. We also need to be able to read the argument provided to `ECHO`, in order to use it in the response.

Since the entire communication follows RESP, we also need to build a decoder for parsing the incoming request, and
an encoder for delivering the response. You can review the complete implementation in the Diff tab — in this
guide we'll highlight the key areas to pay attention to.

We'll decode and capture the request within `command` & `args`. The decoding will be handled by `RESPDecoder`, a utility
class we'll create separately. 

```python
command, *args = RESPDecoder(client_connection).decode()
```

We check the decoded value to see if the command was a `PING` or an `ECHO`. If `PING`, we reuse our response from
the previous stages. We'll look at `ECHO` separately. If neither, we send a
[RESP error response](https://redis.io/docs/reference/protocol-spec/#resp-errors).

```python
if command == b"ping":
    client_connection.send(b"+PONG\r\n")
elif command == b"echo":
    // Handle ECHO
    pass
else:
    client_connection.send(b"-ERR unknown command\r\n")
```

For `ECHO`, we send back the first argument as a RESP-encoded bulk string.

```python
elif command == b"echo":
    client_connection.send(b"$%d\r\n%b\r\n" % (len(args[0]), args[0]))
```

In the solution for this stage, you'll find that we've created a separate utility file for functions related to
RESP. The key function in it is `decode()`. This function reads the first byte from the input and then calls other
functions as needed.

```python
def decode(self):
    data_type_byte = self.connection.read(1)

    if data_type_byte == b"+":
        return self.decode_simple_string()
    elif data_type_byte == b"$":
        return self.decode_bulk_string()
    elif data_type_byte == b"*":
        return self.decode_array()
    else:
        raise Exception(f"Unknown data type byte: {data_type_byte}")
```
