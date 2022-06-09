#[allow(unused_imports)]
use std::env;
#[allow(unused_imports)]
use std::fs;
#[allow(unused_imports)]
use std::net::TcpListener;

fn main() {
    let listener = TcpListener::bind("127.0.0.1:6379").unwrap();
    match listener.accept() {
        Ok((_socket, addr)) => println!("accepted new client: {:?}", addr),
        Err(e) => println!("couldn't accept client: {:?}", e),
    }
}
