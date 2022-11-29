Stage 3 builds on our progress from Stage 2.

Earlier, we were anticipating a single `PING` request, and were accordingly responding back with a single `PONG`.

As an improvement, we'll now monitor for more incoming requests — and each time we get one, we'll respond back with
`PONG`, and go back to waiting for the next one. We can achieve this with an infinite `loop`.

```rust
let mut buf = [0; 512];

loop {
    // Wait for the client to send us a message but ignore the content for now
    let bytes_read = stream.read(&mut buf).unwrap();
    if bytes_read == 0 {
        println!("client closed the connection");
        break;
    }

    stream.write("+PONG\r\n".as_bytes()).unwrap();
}
```

While this approach gets us through the stage, notice how our program is now blocked on the for loop as it waits 
for incoming requests. During this time, the program cannot do anything else (including accepting connections from 
other clients). We'll get around that in the next stage when we tackle concurrent requests.
