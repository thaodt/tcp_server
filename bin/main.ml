open Unix

let buffer_size = 1024
let server_addr = ADDR_INET(inet_addr_loopback, 8080)

let handle_client (client_sock, client_addr) =
  let buffer = Bytes.create buffer_size in
  let rec communication_loop () =
    match recv client_sock buffer 0 buffer_size [] with
    | 0 -> ()  (* Connection closed by client *)
    | bytes_received ->
        let message = Bytes.sub_string buffer 0 bytes_received in
        (match client_addr with
        | ADDR_INET(addr, _) ->
            Printf.printf "Received from %s: %s\n" 
              (string_of_inet_addr addr) message;
        | _ ->
            Printf.printf "Received from unknown address type: %s\n" message);
        ignore (send client_sock buffer 0 bytes_received []);
        communication_loop ()
    | exception Unix_error(ECONNRESET, _, _) -> 
        (match client_addr with
        | ADDR_INET(addr, _) ->
            Printf.printf "Client %s disconnected.\n" 
              (string_of_inet_addr addr)
        | _ ->
            Printf.printf "Client with unknown address type disconnected.\n")
  in
  try
    communication_loop ();
    close client_sock
  with e ->
    close client_sock;
    Printf.eprintf "Error handling client: %s\n" (Printexc.to_string e)


let create_server_socket () =
  Printf.printf "Creating server socket...\n%!";
  let sock = socket PF_INET SOCK_STREAM 0 in
  Printf.printf "Socket created.\n%!";
  setsockopt sock SO_REUSEADDR true;
  Printf.printf "SO_REUSEADDR set.\n%!";
  bind sock server_addr;
  Printf.printf "Socket bound to address.\n%!";
  listen sock 5;
  Printf.printf "Socket listening.\n%!";
  sock

let rec accept_loop server_sock =
  Printf.printf "Waiting for connection...\n%!";
  let (client_sock, client_addr) = accept server_sock in
  (match client_addr with
  | ADDR_INET(addr, _) ->
      Printf.printf "New connection from %s\n%!" (string_of_inet_addr addr);
  | _ ->
      Printf.printf "New connection from unknown address type\n%!");
  ignore (Thread.create handle_client (client_sock, client_addr));
  accept_loop server_sock

let main () =
  Printf.printf "Starting server...\n%!";
  let server_sock = create_server_socket () in
  Printf.printf "TCP server listening on localhost:8080\n%!";
  try
    accept_loop server_sock
  with e ->
    close server_sock;
    Printf.eprintf "Server error: %s\n%!" (Printexc.to_string e)

let () = 
  Printexc.record_backtrace true;
  main ()

