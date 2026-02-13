const std = @import("std");
const net = std.net;

pub fn main() !void {
    const address = net.Address.parseIp4("0.0.0.0", 6379) catch unreachable;
    var server = try address.listen(.{ .reuse_address = true });
    defer server.deinit();
    _ = try server.accept();
}
