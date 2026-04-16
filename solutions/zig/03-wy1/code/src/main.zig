const std = @import("std");
const stdout = std.fs.File.stdout();
const net = std.net;

pub fn main() !void {
    const address = try net.Address.resolveIp("127.0.0.1", 6379);

    var listener = try address.listen(.{
        .reuse_address = true,
    });
    defer listener.deinit();

    while (true) {
        const connection = try listener.accept();
        defer connection.stream.close();

        try stdout.writeAll("accepted new connection");

        var buf: [1024]u8 = undefined;
        while (true) {
            const bytes_read = try connection.stream.read(&buf);
            if (bytes_read == 0) break;
            try connection.stream.writeAll("+PONG\r\n");
        }
    }
}
