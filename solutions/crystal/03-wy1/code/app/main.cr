require "socket"

# Ensure that the program terminates on SIGTERM, https://github.com/crystal-lang/crystal/issues/8687
Signal::TERM.trap { exit }

class YourRedisServer
  def start
    server = TCPServer.new("0.0.0.0", 6379)
    client = server.accept?
    if client
      buf = Bytes.new(1024)
      while (bytes_read = client.read(buf)) > 0
        client << "+PONG\r\n"
        client.flush
      end
    end
  end
end

YourRedisServer.new.start
