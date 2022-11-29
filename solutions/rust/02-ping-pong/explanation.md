Stage 2 is an exercise in reading and responding to requests with `TcpStream`. The official documentation [is here](https://doc.rust-lang.org/std/net/struct.TcpStream.html).

In the first stage, we were just printing a message when a client connected and ignoring the TCP connection (`_stream`).

```rust
Ok(_stream) => {
    println!("accepted new connection");
}
```

As we go further, we want to actually parse the incoming request, and respond suitably. Since we know that the 
client only sends us `PING` at the moment, we can hardcode our response.

First, we stop ignoring the connection and make it mutable so that we can write to it.

```rust
Ok(mut stream) => {
    println!("accepted new connection");
}
```

For this stage, we know that the tester _only_ sends us `PING`, so we don't have to parse the incoming data. We should read it though, so that we wait for the client to send a message before we respond. To do that we create a buffer to hold the incoming message and read into it

```rust
let mut buf = [0; 512];
stream.read(&mut buf).unwrap();
```

As mentioned in the stage instructions, we need to encode the response as a 
[RESP Simple String](https://redis.io/docs/reference/protocol-spec/#resp-simple-strings). The ideal approach is to 
create a RESP encoder function — but for now, since we know that our response will always be `PONG`, we can hardcode 
the response. We will create the function in the upcoming stages.

```rust
stream.write("+PONG\r\n".as_bytes()).unwrap();
```
