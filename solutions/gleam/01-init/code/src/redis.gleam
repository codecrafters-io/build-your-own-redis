import gleam/io
import gleam/result
import glisten/socket/options.{ActiveMode, Passive}
import glisten/tcp

pub fn main() {
  use _listener <- result.then(tcp.listen(8000, [ActiveMode(Passive)]))

  Ok(Nil)
}
