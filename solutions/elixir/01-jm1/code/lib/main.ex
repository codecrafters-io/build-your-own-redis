defmodule CLI do
  def main(_args) do
    {:ok, server} = :gen_tcp.listen(6379, [:binary, active: false, reuseaddr: true])
    {:ok, _client} = :gen_tcp.accept(server)
  end
end
