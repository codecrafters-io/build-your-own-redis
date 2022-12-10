In Stage 5, we explore streams processing.

In previous stages, the incoming client requests were always `PING`, and so we were able to hardcode our responses 
as `PONG`. Adding support for `ECHO` means that we first need to determine whether the incoming request is an `ECHO` 
or a `PING`. We also need to be able to read the argument provided to `ECHO`, in order to use it in the response.

Since the entire communication follows RESP, we also need to build a decoder for parsing the incoming request, and 
an encoder for delivering the response. You can review the complete implementation in the Diff tab — in this 
guide we'll highlight the key areas to pay attention to.

There is a new module called `resp` which provides a `RespConnection`. This new struct handles the reading and 
writing of messages from the TCP stream. Internally it reads from the socket until a complete input has been 
received then gives us back the value so that we can decide what to do. The `handle_connection` function now becomes -

```rust
async fn handle_connection(stream: TcpStream) -> Result<()> {
    let mut conn = resp::RespConnection::new(stream);

    loop {
        let value = conn.read_value().await?;

        // TODO check the value and respond
    }

    Ok(())
}
```

We assume that the incoming message will be represented as a `Value::Array` and use a utility function to get the 
head of the array, the command, and tail of it, the arguments. We check the command to see if it was a `PING` or an `ECHO`. 
If `PING`, we reuse our response from the previous stages. If `ECHO` we send back the first argument we received.
Otherwise, send back an error message.

```rust
if let Some(value) = value {
    let (command, args) = value.to_command()?;
    let response = match command.to_ascii_lowercase().as_ref() {
        "ping" => resp::Value::String("PONG".to_string()),
        "echo" => args.first().unwrap().clone(),
        _ => resp::Value::Error(format!("command not implemented: {}", command))
    };

    conn.write_value(response).await?;
} else {
    break;
}
```

You should review the `resp` module as needed but the key function is `parse_message`. This function reads the first byte from the input and then calls other functions as needed.

```rust
fn parse_message(buffer: BytesMut) -> Result<Option<(Value, usize)>> {
    match buffer[0] as char {
        '+' => decode_simple_string(buffer),
        '*' => decode_array(buffer),
        '$' => decode_bulk_string(buffer),
        _ => {
            return Err(Error::msg("unrecognised message type"));
        }
    }
}
```
