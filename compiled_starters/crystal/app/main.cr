require "socket"

# Ensure that the program terminates on SIGTERM, https://github.com/crystal-lang/crystal/issues/8687
Signal::TERM.trap { exit }

class YourRedisServer
  def start
    # You can use print statements as follows for debugging, they'll be visible when running tests.
    puts("Logs from your program will appear here!")

    # Uncomment the code below to pass the first stage the first stage
    # server = TCPServer.new("0.0.0.0", 6379)
    # client = server.accept?
  end
end

YourRedisServer.new.start
