require "socket"

# Ensure that the program terminates on SIGTERM, https://github.com/crystal-lang/crystal/issues/8687
Signal::TERM.trap { exit }

class YourRedisServer
  def start
    server = TCPServer.new("0.0.0.0", 6379, reuse_port: true)
    client = server.accept?
  end
end

YourRedisServer.new.start
