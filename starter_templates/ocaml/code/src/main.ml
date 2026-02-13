let () =
  (* You can use print statements as follows for debugging, they'll be visible when running tests. *)
  Printf.eprintf "Logs from your program will appear here!\n";

  (* Uncomment the code below to pass the first stage *)
  (* let server_fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in *)
  (* Unix.setsockopt server_fd Unix.SO_REUSEADDR true; *)
  (* Unix.bind server_fd (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 6379)); *)
  (* Unix.listen server_fd 128; *)
  (* ignore (Unix.accept server_fd); *)
  ()
