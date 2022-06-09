const net = require("net");

const server = net.createServer(socket => {
  socket.pipe(socket);
});

server.listen(6379, '127.0.0.1');
