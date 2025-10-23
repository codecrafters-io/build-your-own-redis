#![allow(unused_imports)]
use std::net::TcpListener;

fn main() {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    println!("Logs from your program will appear here!");

    // Uncomment this block to pass the first stage
    //
    // let listener = TcpListener::bind("127.0.0.1:6379").unwrap();
    //
    // for stream in listener.incoming() {
    //     match stream {
    //         Ok(_stream) => {
    //             println!("accepted new connection");
    //         }
    //         Err(e) => {
    //             println!("error: {}", e);
    //         }
    //     }
    // }
}
