In stage 4 you need to handle concurrent clients, which means that the server has to accept more than one connection at a time.

The `network-simple` library already handles concurrency for us, so there almost nothing to do in the Haskell world for this stage, apart from removing the manual closing of the connection.
The `serve` function closes a connection automatically and frees up resources, even in case of errors.
However, as previously mentioned, the `do` notation requires an expression at the end, hence we add a `putStrLn` with a custom output at the end.

```haskell
putStrLn $ "disconnected client: " ++ show address
```

You could do an alternative implementation if you do not want to use the `network-simple` library, but the more powerful `network` library.
There, you have to implement concurrency yourself, which you can do by using `forkFinally`.
The [network library description](https://hackage.haskell.org/package/network/docs/Network-Socket.html) includes an example if you want to try this.
