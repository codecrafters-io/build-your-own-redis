In stage 2 you use the `network-simple` Haskell library that helps with listening to a TCP connection, sending and receiving requests.
You can find the documentation for [network-simple](https://hackage.haskell.org/package/network-simple) on Hackage.

In later stages, we will make heavy use of other libraries and their functions.
The `package.yaml` file contains a list of all libraries that we are going to use in this course, so you may get some hints for your implementation.

The `serve` function from the `network-simple` library opens a connection on the provided port (Redis listens on port 6379).
By using a `lambda` function in combination with a monadic `do` expression you can accept a connection request.

```haskell
serve HostAny port $ \(socket, address) -> do
```

We will encounter the topic of Monads in subsequent stages again, so it may be helpful to read up on this topic if you are not yet familiar with it.
The Haskell Wiki provides an extensive description of Monads, but you may well just focus on the [do notation](https://wiki.haskell.org/All_About_Monads#Do_notation) for a start.

The information about the connection is stored in the `socket` variable.
Also, the connected IP address is preserved, but only for debugging purposes so you can check locally if a connection is successful.

In this stage, the input from the client is simply discarded, which is achieved by using the underscore `_`.
We use a second library to handle requests and send information back: the `network` library provides a package named `Network.Socket.ByteString` that exposes two functions, `recv` and `send`.

```haskell
_ <- recv socket 2048
```

Since only a simple `PING` is sent by the client, there is no need to parse the incoming request.
Therefore, we can respond with `PONG`.

```haskell
_ <- send socket "+PONG\r\n"
```

You may wonder why there are more characters before and after `PONG`.
Redis has its own encoding scheme, called Redis serialization protocol or RESP.
The way a simple string as PONG is sent over the network and received by a Redis client is by using so called [RESP Simple Strings](https://redis.io/docs/reference/protocol-spec/#resp-simple-strings).

In later stages, we will create an encoding function that takes care of this conversion.

After the response is sent, the connection is simply closed and the program terminates.

```haskell
closeSock socket
```