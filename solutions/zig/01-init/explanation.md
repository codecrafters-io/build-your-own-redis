The entry point for your Redis implementation is in `src/main.zig`.

Study and uncomment the relevant code: 

```zig
// Uncomment this block to pass the first stage
const net = std.net;
```

```zig
// Uncomment this block to pass the first stage

const address = try net.Address.resolveIp("127.0.0.1", 6379);

var listener = try address.listen(.{
    .reuse_address = true,
});
defer listener.deinit();

while (true) {
    const connection = try listener.accept();

    try stdout.print("accepted new connection", .{});
    connection.stream.close();
}
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
