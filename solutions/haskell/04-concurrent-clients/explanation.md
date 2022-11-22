In stage 4 you need to handle concurrent clients, which means that the server has to accept more than one connection at a time.

The `network-simple` library already handles concurrency for us, so there is nothing to do in the Haskell world for this stage.

There exists an alternative implementation if you are not using the `network-simple` library but the `network` library.
There, you have to implement concurrency yourself, which you can do by using `forkFinally`.
The [network library description](https://hackage.haskell.org/package/network-3.1.2.7/docs/Network-Socket.html) includes an example if you want to try this.
