import gleam/io
import gleam/result
import glisten/socket/options.{ActiveMode, Passive}
import glisten/tcp

pub fn main() {
  use listener <- result.then(tcp.listen(6379, [ActiveMode(Passive)]))
  use _socket <- result.then(tcp.accept(listener))

  Ok(Nil)
}
