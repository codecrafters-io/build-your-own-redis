The entry point for your Redis implementation is in `src/main.gleam`.

Study and uncomment the relevant code: 

```gleam
// Uncomment this block to pass the first stage

let assert Ok(_) =
  glisten.handler(fn(_conn) { #(Nil, None) }, fn(_msg, state, _conn) {
    io.println("Received message!")
    actor.continue(state)
  })
  |> glisten.serve(6379)

process.sleep_forever()
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
