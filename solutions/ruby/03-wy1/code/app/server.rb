require "socket"

class YourRedisServer
  def initialize(port)
    @port = port
  end

  def start
    server = TCPServer.new(@port)
    client = server.accept
    loop do
      data = client.readpartial(1024)
      client.write("+PONG\r\n")
    rescue EOFError
      break
    end
  end
end

YourRedisServer.new(6379).start
