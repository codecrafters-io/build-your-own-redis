Stage 4 requires switching to Tokio so that we can handle multiple clients concurrently. There is an official tutorial for Tokio [to get you started](https://tokio.rs/tokio/tutorial).

Let's start by moving our responder into its own function. It needs a `TcpStream` as an argument.

```rust
async fn handle_connection(stream: &mut TcpStream) -> Result<()> {
    let mut buf = [0; 512];

    loop {
        // Wait for the client to send us a message but ignore the content for now
        let bytes_read = stream.read(&mut buf).await?;
        if bytes_read == 0 {
            println!("client closed the connection");
            break;
        }

        stream.write("+PONG\r\n".as_bytes()).await?;
    }

    Ok(())
}
```

Notice that the `handle_connection` function is marked as `async` and the `read`/`write` calls are `await`ed. These changes allow Tokio to suspend and resume our connection handler at the right times, and do work on tasks for other clients while ours is suspended.

We need to annotate the `main` function with `#[tokio::main]` to let Tokio start a runtime before our main function does any work.

```rust
#[tokio::main]
async fn main() -> Result<()> {
	// ...
}
```

Within `main`, we'll now use a `loop` to accept multiple connections. For each client connection that we accept, we'll 
spawn a task that handles the connection.

```rust
#[tokio::main]
async fn main() -> Result<()> {
    let mut listener = TcpListener::bind("127.0.0.1:6379").await?;

    loop {
        let incoming = listener.accept().await;
        match incoming {
            Ok((mut stream, _)) => {
                println!("accepted new connection");
                tokio::spawn(async move {
                    handle_connection(&mut stream).await.unwrap();
                });
            }
            Err(e) => {
                println!("error: {}", e);
            }
        }
    }
}
```

Using a tokio task, with `tokio::spawn`, makes the program able to accept and handle new incoming connections in parallel, instead of 
blocking the program by sequentially handling one connection at a time.
