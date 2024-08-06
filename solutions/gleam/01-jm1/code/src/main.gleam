import gleam/io

import gleam/erlang/process
import gleam/option.{None}
import gleam/otp/actor
import glisten

pub fn main() {
  let assert Ok(_) =
    glisten.handler(fn(_conn) { #(Nil, None) }, fn(_msg, state, _conn) {
      io.println("Received message!")
      actor.continue(state)
    })
    |> glisten.serve(6379)

  process.sleep_forever()
}
