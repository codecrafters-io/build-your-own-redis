The entry point for your Redis implementation is in `main.rs`.

Study and uncomment the relevant code: 

```rust
// Uncomment this block to pass the first stage
let listener = TcpListener::bind("127.0.0.1:6379").unwrap();
match listener.accept() {
    Ok((_socket, addr)) => println!("accepted new client: {:?}", addr),
    Err(e) => println!("couldn't accept client: {:?}", e),
}
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
