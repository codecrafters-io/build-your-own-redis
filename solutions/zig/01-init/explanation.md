The entry point for your Redis implementation is in `src/main.rs`.

Study and uncomment the relevant code:

```zig
// Uncomment this block to pass the first stage
const net = std.net;
```

```zig
// Uncomment this block to pass the first stage

const address = try net.Address.resolveIp("127.0.0.1", 6379);

const listener = try address.listen(.{
    .reuse_address = true,
});

while (true) {
    const connection = try listener.accept();
    _ = connection;

    try stdout.print("accepted new connection", .{});
}
```

Push your changes to pass the first stage:

```
git add .
git commit -m "pass 1st stage" # any msg
git push origin master
```
