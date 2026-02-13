package codecrafters_redis

import java.net.ServerSocket

object Main extends App {
  val serverSocket = new ServerSocket(6379)
  serverSocket.setReuseAddress(true)
  serverSocket.accept() // wait for client
}
