const std = @import("std");
const stdout = std.fs.File.stdout();
const net = std.net;

pub fn main() !void {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    try stdout.writeAll("Logs from your program will appear here!");

    // Uncomment the code below to pass the first stage the first stage
    //
    // const address = try net.Address.resolveIp("127.0.0.1", 6379);
    //
    // var listener = try address.listen(.{
    //     .reuse_address = true,
    // });
    // defer listener.deinit();
    //
    // while (true) {
    //     const connection = try listener.accept();
    //
    //     try stdout.writeAll("accepted new connection");
    //     connection.stream.close();
    // }
}
