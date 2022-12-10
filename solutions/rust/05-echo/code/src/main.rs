use anyhow::Result;
use tokio::net::{TcpListener, TcpStream};

mod resp;

#[tokio::main]
async fn main() -> Result<()> {
    let mut listener = TcpListener::bind("127.0.0.1:6379").await?;

    loop {
        let incoming = listener.accept().await;
        match incoming {
            Ok((stream, _)) => {
                println!("accepted new connection");
                tokio::spawn(async move {
                    handle_connection(stream).await.unwrap();
                });
            }
            Err(e) => {
                println!("error: {}", e);
            }
        }
    }
}

async fn handle_connection(stream: TcpStream) -> Result<()> {
    let mut conn = resp::RespConnection::new(stream);

    loop {
        let value = conn.read_value().await?;

        if let Some(value) = value {
            let (command, args) = value.to_command()?;
            let response = match command.to_ascii_lowercase().as_ref() {
                "ping" => resp::Value::SimpleString("PONG".to_string()),
                "echo" => args.first().unwrap().clone(),
                _ => resp::Value::Error(format!("command not implemented: {}", command)),
            };

            conn.write_value(response).await?;
        } else {
            break;
        }
    }

    Ok(())
}
