open Unix

let () =
  let server_socket = socket PF_INET SOCK_STREAM 0 in
  setsockopt server_socket SO_REUSEADDR true;
  bind server_socket (ADDR_INET (inet_addr_of_string "127.0.0.1", 6379));
  listen server_socket 1;

  let (client_socket, _) = accept server_socket in

  let buffer = Bytes.create 1024 in
  let rec loop () =
    let bytes_read = read client_socket buffer 0 1024 in
    if bytes_read > 0 then begin
      let response = "+PONG\r\n" in
      ignore (write_substring client_socket response 0 (String.length response));
      loop ()
    end
  in
  loop ();

  close client_socket;
  close server_socket
