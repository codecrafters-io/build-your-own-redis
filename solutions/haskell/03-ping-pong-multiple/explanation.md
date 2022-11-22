In stage 3 you need to continually respond to `PING` requests, rather than closing the connection after responding just once.

You will want something akin to `while(true)`, but for the `IO ()` type. This can be achieved by the `forever` function, which is part of the `Control.Monad` package that is in the `base` library.

As the package name already implies, this is another monadic function, for which you can, of course, use the handy `do` expression.

```haskell
_ <- forever $ do
```

Since the `do` expression requires to finish with an expression itself, we cannot just pack the `recv` and `send` functions into the `forever` loop.

You noticed, that with the arrow `<-` we assign a return value to a variable name (in our case we discard the value using the underscore `_`).
However, the `send` function is an expression itself.
Therefore, if we remove the assignment of the return value from the `send` function we get a valid expression with which we can terminate a do expression.

```haskell
_ <- forever $ do
    _ <- recv socket 2048
    send socket "+PONG\r\n"
```

[This explanation](https://en.wikibooks.org/wiki/Haskell/do_notation) on the do notation goes into more detail why an expression is required at the end (think lambda function).