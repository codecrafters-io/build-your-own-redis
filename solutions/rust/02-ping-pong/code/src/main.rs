use std::net::TcpListener;
use bytes::BytesMut;
use std::io::{Read, Write};

fn main() {
    let listener = TcpListener::bind("127.0.0.1:6379").unwrap();

    for stream in listener.incoming() {
        match stream {
            Ok(mut stream) => {
                println!("accepted new connection");

                // Wait for the client to send us a message but ignore the content for now
                let mut buf = BytesMut::with_capacity(512);
                stream.read(&mut buf).unwrap();

                stream.write("+PONG\r\n".as_bytes()).unwrap();
            }
            Err(e) => {
                println!("error: {}", e);
            }
        }
    }
}
