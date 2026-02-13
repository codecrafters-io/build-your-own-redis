let () =
  let server_fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server_fd Unix.SO_REUSEADDR true;
  Unix.bind server_fd (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 6379));
  Unix.listen server_fd 128;
  ignore (Unix.accept server_fd);
  ()
