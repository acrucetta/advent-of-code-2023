

let digits = List.range 0 10 |> List.map ~f:Int.to_string

let explode s =
  s
  |> String.to_list
;;

let lines input =
  input
  |> Str.split (Str.regexp "\n")
;;

let extract_numbers line =
  line
  |> explode
  |> List.filter ~f:(fun c -> Char.is_digit c)
;;


let part1 input =
  let lines = lines input in
  let first_line = List.hd_exn lines in
  let first_numbers = extract_numbers first_line in
  print_endline (String.concat ~sep:"" first_numbers);
  -1
;;

let part2 input =
  -1
;;