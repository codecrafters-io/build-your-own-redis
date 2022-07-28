const net = require("net");

const server = net.createServer((connection) => {
  // Handle connection
});

server.listen(6379, "127.0.0.1");
