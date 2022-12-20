use anyhow::{Error, Result};
use bytes::BytesMut;
use tokio::{io::AsyncReadExt, io::AsyncWriteExt, net::TcpStream};

const CARRIAGE_RETURN: u8 = '\r' as u8;
const NEWLINE: u8 = '\n' as u8;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Value {
    /// Null bulk reply, `$-1\r\n`
    Null,
    /// For Simple Strings the first byte of the reply is "+".
    SimpleString(String),
    /// For Errors the first byte of the reply is "-".
    Error(String),
    /// For Bulk Strings the first byte of the reply is "$".
    BulkString(String),
    /// For Arrays the first byte of the reply is "*".
    Array(Vec<Value>),
}

impl Value {
    pub fn to_command(&self) -> Result<(String, Vec<Value>)> {
        match self {
            Value::Array(items) => {
                return Ok((
                    items.first().unwrap().unwrap_bulk(),
                    items.clone().into_iter().skip(1).collect(),
                ));
            }
            _ => Err(Error::msg("not an array")),
        }
    }

    fn unwrap_bulk(&self) -> String {
        match self {
            Value::BulkString(str) => str.clone(),
            _ => panic!("not a bulk string"),
        }
    }

    pub fn encode(self) -> String {
        match &self {
            Value::Null => "$-1\r\n".to_string(),
            Value::SimpleString(s) => format!("+{}\r\n", s.as_str()),
            Value::Error(msg) => format!("-{}\r\n", msg.as_str()),
            Value::BulkString(s) => format!("${}\r\n{}\r\n", s.chars().count(), s),
            // The other cases are not required
            _ => panic!("value encode not implemented for: {:?}", self),
        }
    }
}

pub struct RespConnection {
    stream: TcpStream,
    buffer: BytesMut,
}

impl RespConnection {
    pub fn new(stream: TcpStream) -> Self {
        return RespConnection {
            stream,
            buffer: BytesMut::with_capacity(512),
        };
    }

    pub async fn read_value(&mut self) -> Result<Option<Value>> {
        loop {
            let bytes_read = self.stream.read_buf(&mut self.buffer).await?;

            // Connection closed
            if bytes_read == 0 {
                return Ok(None);
            }

            if let Some((value, _)) = parse_message(self.buffer.split())? {
                return Ok(Some(value));
            }
        }
    }

    pub async fn write_value(&mut self, value: Value) -> Result<()> {
        self.stream.write(value.encode().as_bytes()).await?;

        Ok(())
    }
}

fn parse_message(buffer: BytesMut) -> Result<Option<(Value, usize)>> {
    match buffer[0] as char {
        '+' => decode_simple_string(buffer),
        '*' => decode_array(buffer),
        '$' => decode_bulk_string(buffer),
        _ => Err(Error::msg("unrecognised message type")),
    }
}

fn decode_simple_string(buffer: BytesMut) -> Result<Option<(Value, usize)>> {
    if let Some((line, len)) = read_until_crlf(&buffer[1..]) {
        let str = parse_string(line)?;

        Ok(Some((Value::SimpleString(str), len + 1)))
    } else {
        Ok(None)
    }
}

fn decode_array(buffer: BytesMut) -> Result<Option<(Value, usize)>> {
    let (array_length, mut bytes_consumed) =
        if let Some((line, len)) = read_until_crlf(&buffer[1..]) {
            let array_length = parse_integer(line)?;

            (array_length, len + 1)
        } else {
            return Ok(None);
        };

    let mut items: Vec<Value> = Vec::new();
    for _ in 0..array_length {
        if let Some((v, len)) = parse_message(BytesMut::from(&buffer[bytes_consumed..]))? {
            items.push(v);
            bytes_consumed += len
        } else {
            return Ok(None);
        }
    }

    return Ok(Some((Value::Array(items), bytes_consumed)));
}

fn decode_bulk_string(buffer: BytesMut) -> Result<Option<(Value, usize)>> {
    let (bulk_length, bytes_consumed) = if let Some((line, len)) = read_until_crlf(&buffer[1..]) {
        let bulk_length = parse_integer(line)?;

        (bulk_length, len + 1)
    } else {
        return Ok(None);
    };

    let end_of_bulk = bytes_consumed + (bulk_length as usize);
    let end_of_bulk_line = end_of_bulk + 2;

    return if end_of_bulk_line <= buffer.len() {
        Ok(Some((
            Value::BulkString(parse_string(&buffer[bytes_consumed..end_of_bulk])?),
            end_of_bulk_line,
        )))
    } else {
        Ok(None)
    };
}

fn read_until_crlf(buffer: &[u8]) -> Option<(&[u8], usize)> {
    for i in 1..buffer.len() {
        if buffer[i - 1] == CARRIAGE_RETURN && buffer[i] == NEWLINE {
            return Some((&buffer[0..(i - 1)], i + 1));
        }
    }

    return None;
}

fn parse_string(bytes: &[u8]) -> Result<String> {
    String::from_utf8(bytes.to_vec()).map_err(|_| Error::msg("Could not parse string"))
}

fn parse_integer(bytes: &[u8]) -> Result<i64> {
    let str_integer = parse_string(bytes)?;
    (str_integer.parse::<i64>()).map_err(|_| Error::msg("Could not parse integer"))
}
