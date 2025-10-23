open Unix

let () =
  (* You can use print statements as follows for debugging, they'll be visible when running tests. *)
  Printf.eprintf "Logs from your program will appear here!\n";

  (* Create a TCP server socket *)
  let server_socket = socket PF_INET SOCK_STREAM 0 in
  setsockopt server_socket SO_REUSEADDR true;
  bind server_socket (ADDR_INET (inet_addr_of_string "127.0.0.1", 6379));
  listen server_socket 1;

  (* Uncomment this block to pass the first stage *)
  (* let (client_socket, _) = accept server_socket in *)
  (* close client_socket; *)
  (* close server_socket *)
