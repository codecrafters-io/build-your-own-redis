The entry point for your Redis implementation is in `Sources/main.swift`.

Study and uncomment the relevant code: 

```swift
// Uncomment this block to pass the first stage
// Bind the server to port 6379 and start accepting connections
let channel = try serverBootstrap.bind(host: "localhost", port: 6379).wait()
print("Server started and listening on \(channel.localAddress!)")
try channel.closeFuture.wait()
print("Server closed")
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
