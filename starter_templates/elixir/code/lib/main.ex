defmodule CLI do
  def main(_args) do
    # You can use print statements as follows for debugging, they'll be visible when running tests.
    IO.puts(:stderr, "Logs from your program will appear here!")

    # Uncomment the code below to pass the first stage
    #
    # {:ok, server} = :gen_tcp.listen(6379, [:binary, active: false, reuseaddr: true])
    # {:ok, _client} = :gen_tcp.accept(server)
  end
end
