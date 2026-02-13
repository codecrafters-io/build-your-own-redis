import java.net.ServerSocket

fun main(args: Array<String>) {
    val serverSocket = ServerSocket(6379)
    serverSocket.reuseAddress = true
    val clientSocket = serverSocket.accept() // Wait for a client to connect
}
