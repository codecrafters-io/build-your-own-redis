The entry point for your Redis implementation is in `src/main.rs`.

Study and uncomment the relevant code: 

```rust
// Uncomment this block to pass the first stage

let listener = TcpListener::bind("127.0.0.1:6379").unwrap();

for stream in listener.incoming() {
    match stream {
        Ok(_stream) => {
            println!("accepted new connection");
        }
        Err(e) => {
            println!("error: {}", e);
        }
    }
}
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
