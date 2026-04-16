const std = @import("std");

pub fn main(init: std.process.Init) !void {
    const io = init.io;

    const address = try std.Io.net.IpAddress.parseIp4("127.0.0.1", 6379);

    var server = try address.listen(io, .{
        .reuse_address = true,
    });
    defer server.deinit(io);

    const connection = try server.accept(io);
    defer connection.close(io);

    var connection_writer = connection.writer(io, &.{});
    try connection_writer.interface.writeAll("+PONG\r\n");
}
