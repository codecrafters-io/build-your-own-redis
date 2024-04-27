const std = @import("std");
// const net = std.net;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    // You can use print statements as follows for debugging, they'll be visible when running tests.
    try stdout.print("Logs from your program will appear here!", .{});

    // Uncomment this block to pass the first stage
    //
    // const address = try net.Address.resolveIp("127.0.0.1", 6379);
    //
    // const listener = try address.listen(.{
    //     .reuse_address = true,
    // });
    //
    // while (true) {
    //     const connection = try listener.accept();
    //     _ = connection;
    //
    //     try stdout.print("accepted new connection", .{});
    // }
}
