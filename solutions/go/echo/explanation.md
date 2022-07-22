In Stage 5, we explore streams processing.

In previous stages, the incoming client requests were always `PING`, and so we were able to hardcode our responses 
as `PONG`. Adding support for `ECHO` means that we first need to determine whether the incoming request is an `ECHO` 
or a `PING`. We also need to be able to read the argument provided to `ECHO`, in order to use it in the response.

Since the entire communication follows RESP, we also need to build a decoder for parsing the incoming request, and 
an encoder for delivering the response. You can review the complete implementation in the Diff tab — in this 
guide we'll highlight the key areas to pay attention to.

We'll decode and capture the request within `value`. The decoding will be handled by `DecodeRESP`, a utility 
function we'll create separately. If the decoding results in an error, we capture the error and `return` to 
observe other requests.

```go
value, err := DecodeRESP(bufio.NewReader(conn))

if err != nil {
  fmt.Println("Error decoding RESP: ", err.Error())
  return
}
```

We check the decoded value to see if the command was a `PING` or an `ECHO`. If `PING`, we reuse our response from 
the previous stages. We'll look at `ECHO` separately. If neither, we send a 
[RESP error response](https://redis.io/docs/reference/protocol-spec/#resp-errors).

```go
command := value.Array()[0].String()
args := value.Array()[1:]

switch command {
  case "ping":
    conn.Write([]byte("+PONG\r\n"))
  case "echo":
    // handle ECHO
  default:
    conn.Write([]byte("-ERR unknown command '" + command + "'\r\n"))
}
```

For `ECHO`, we send back the first argument as a RESP-encoded bulk string.

```go
case "echo":
conn.Write([]byte(fmt.Sprintf("$%d\r\n%s\r\n", len(args[0].String()), args[0].String())))
```

In the solution for this stage, you'll find that we've created a separate utility file for functions related to 
RESP. The key function in it is `DecodeRESP`. This function reads the first byte from the input and then calls other 
functions as needed.

```go
func DecodeRESP(byteStream *bufio.Reader) (Value, error) {
  dataTypeByte, err := byteStream.ReadByte()
  if err != nil {
    return Value{}, err
  }

  switch string(dataTypeByte) {
    case "+":
      return decodeSimpleString(byteStream)
    case "$":
      return decodeBulkString(byteStream)
    case "*":
      return decodeArray(byteStream)
  }

  return Value{}, fmt.Errorf("invalid RESP data type byte: %s", string(dataTypeByte))
}
```
