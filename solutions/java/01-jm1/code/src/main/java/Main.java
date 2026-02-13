import java.io.IOException;
import java.net.ServerSocket;

public class Main {
  public static void main(String[] args){
    ServerSocket serverSocket = null;
    try {
      serverSocket = new ServerSocket(6379);
      serverSocket.setReuseAddress(true);
      serverSocket.accept(); // Wait for connection
    } catch (IOException e) {
      System.out.println("IOException: " + e.getMessage());
    }
  }
}
