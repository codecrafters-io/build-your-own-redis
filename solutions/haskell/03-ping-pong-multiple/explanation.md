In stage 4 you need to handle concurrent clients, which means that the server has to accept more than one connection at a time.

The `network-simple` library already handles concurrency for us, so the only thing we need is a way to continually listen and accept new client connections.
Something akin to `while(true)`, but for the `IO ()` type.

This can be achieve by the `forever` function, which is part of the `Control.Monad` package that is in the `base` library.
As the package name already implies, this is another monadic function, for which you can, of course, use the handy `do` expression.

```haskell
_ <- forever $ do
```

Since the `do` expression requires to finish with an expression itself, we cannot just pack the `recv` and `send` functions into the `forever` loop.
Therefore, we simply remove the return value from the `send` function which we discarded anyway.

```haskell
_ <- forever $ do
    _ <- recv socket 2048
    send socket "+PONG\r\n"
```