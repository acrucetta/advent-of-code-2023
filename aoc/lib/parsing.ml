(* Include this line only if you're using Core *)
  open Core

(* read lines in a file *)
let file = "input.txt"
let message = "Hello, world!"

let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let () =
  (* Print the contents in input.txt *)
  (* Change to 'List.iter' without '~f' if not using Core *)
  List.iter (read_lines file) ~f:(fun line -> print_endline line);
  (* Print the message *)
  print_endline message
