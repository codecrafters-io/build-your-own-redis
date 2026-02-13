import gleam/io

type Socket

@external(erlang, "gen_tcp", "listen")
fn listen(port: Int, options: List(Nil)) -> Result(Socket, Nil)

@external(erlang, "gen_tcp", "accept")
fn accept(socket: Socket) -> Result(Socket, Nil)

pub fn main() {
  let assert Ok(listener) = listen(6379, [])
  let assert Ok(_conn) = accept(listener)
}
