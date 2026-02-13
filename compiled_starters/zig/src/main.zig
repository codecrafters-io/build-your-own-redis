const std = @import("std");
const net = std.net;

pub fn main() !void {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    std.debug.print("Logs from your program will appear here!\n", .{});

    // Uncomment the code below to pass the first stage
    //
    // const address = net.Address.parseIp4("0.0.0.0", 6379) catch unreachable;
    // var server = try address.listen(.{ .reuse_address = true });
    // defer server.deinit();
    // _ = try server.accept();
}
