import * as net from "node:net";

const server: net.Server = net.createServer((connection: net.Socket) => {
  // Handle connection
});

server.listen(6379, "127.0.0.1");

