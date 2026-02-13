require "socket"

server = TCPServer.new("0.0.0.0", 6379)
server.accept
