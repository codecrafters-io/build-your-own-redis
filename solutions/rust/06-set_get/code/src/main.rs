use anyhow::Result;
use resp::Value::{BulkString, Error, Null, SimpleString};
use std::sync::{Arc, Mutex};
use store::Store;
use tokio::net::{TcpListener, TcpStream};

mod resp;
mod store;

#[tokio::main]
async fn main() -> Result<()> {
    let mut listener = TcpListener::bind("127.0.0.1:6379").await?;

    let main_store = Arc::new(Mutex::new(Store::new()));

    loop {
        let incoming = listener.accept().await;
        let client_store = main_store.clone();
        match incoming {
            Ok((stream, _)) => {
                println!("accepted new connection");
                tokio::spawn(async move {
                    handle_connection(stream, client_store).await.unwrap();
                });
            }
            Err(e) => {
                println!("error: {}", e);
            }
        }
    }
}

async fn handle_connection(stream: TcpStream, client_store: Arc<Mutex<Store>>) -> Result<()> {
    let mut conn = resp::RespConnection::new(stream);

    loop {
        let value = conn.read_value().await?;

        if let Some(value) = value {
            let (command, args) = value.to_command()?;
            let response = match command.to_ascii_lowercase().as_ref() {
                "ping" => SimpleString("PONG".to_string()),
                "echo" => args.first().unwrap().clone(),
                "get" => {
                    if let Some(BulkString(key)) = args.get(0) {
                        if let Some(value) = client_store.lock().unwrap().get(key.clone()) {
                            SimpleString(value)
                        } else {
                            Null
                        }
                    } else {
                        Error("Get requires one argument".to_string())
                    }
                }
                "set" => {
                    if let (Some(BulkString(key)), Some(BulkString(value))) =
                        (args.get(0), args.get(1))
                    {
                        client_store.lock().unwrap().set(key.clone(), value.clone());
                        SimpleString("OK".to_string())
                    } else {
                        Error("Set requires two arguments".to_string())
                    }
                }
                _ => Error(format!("command not implemented: {}", command)),
            };

            conn.write_value(response).await?;
        } else {
            break;
        }
    }

    Ok(())
}
