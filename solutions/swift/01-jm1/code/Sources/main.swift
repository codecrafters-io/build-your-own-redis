import Foundation

let serverSocket = socket(AF_INET, Int32(SOCK_STREAM.rawValue), 0)
guard serverSocket >= 0 else {
    print("Failed to create socket")
    exit(1)
}

var reuse = Int32(1)
setsockopt(serverSocket, SOL_SOCKET, SO_REUSEADDR, &reuse, socklen_t(MemoryLayout<Int32>.size))

var addr = sockaddr_in()
addr.sin_family = sa_family_t(AF_INET)
addr.sin_port = UInt16(6379).bigEndian
addr.sin_addr.s_addr = inet_addr("0.0.0.0")

let bindResult = withUnsafePointer(to: &addr) {
    $0.withMemoryRebound(to: sockaddr.self, capacity: 1) {
        bind(serverSocket, $0, socklen_t(MemoryLayout<sockaddr_in>.size))
    }
}
guard bindResult >= 0 else {
    print("Failed to bind to port 6379")
    exit(1)
}

listen(serverSocket, 1)

let _ = withUnsafeMutablePointer(to: &addr) {
    $0.withMemoryRebound(to: sockaddr.self, capacity: 1) {
        var addrLen = socklen_t(MemoryLayout<sockaddr_in>.size)
        return accept(serverSocket, $0, &addrLen)
    }
}
