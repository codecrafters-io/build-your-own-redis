const net = require("net");

const server = net.createServer((socket) => {
  socket.on("data", () => {
    socket.write("+PONG\r\n");
  });
});

server.listen(6379, "127.0.0.1");
