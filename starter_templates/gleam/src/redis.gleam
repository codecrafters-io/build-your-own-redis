import gleam/io
import gleam/result
import glisten/socket/options.{ActiveMode, Passive}
import glisten/tcp

pub fn main() {
  // You can use print statements as follows for debugging, they'll be visible when running tests.
  io.println("Logs from your program will appear here!")

  use _listener <- result.then(tcp.listen(8000, [ActiveMode(Passive)]))

  Ok(Nil)
}
