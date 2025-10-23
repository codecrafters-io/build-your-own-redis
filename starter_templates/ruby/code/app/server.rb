require "socket"

class YourRedisServer
  def initialize(port)
    @port = port
  end

  def start
    # You can use print statements as follows for debugging, they'll be visible when running tests.
    puts("Logs from your program will appear here!")

    # Uncomment the code below to pass the first stage
    # server = TCPServer.new(@port)
    # client = server.accept
  end
end

YourRedisServer.new(6379).start
