import gleam/io

import gleam/erlang/process
import gleam/option.{None}
import gleam/otp/actor
import glisten

pub fn main() {
  // Ensures gleam doesn't complain about unused imports in stage 1 (feel free to remove this!)
  let _ = glisten.handler
  let _ = glisten.serve
  let _ = process.sleep_forever
  let _ = actor.continue
  let _ = None

  let assert Ok(_) =
    glisten.handler(fn(_conn) { #(Nil, None) }, fn(_msg, state, _conn) {
      io.println("Received message!")
      actor.continue(state)
    })
    |> glisten.serve(6379)

  process.sleep_forever()
}
