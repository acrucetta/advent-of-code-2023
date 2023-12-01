

let digits = List.range 0 10 |> List.map ~f:Int.to_string

let lines input =
  input
  |> Str.split (Str.regexp "\n")
;;

let extract_numbers line =
  line
  |> String.to_list
  |> List.filter ~f:(fun c -> Char.is_digit c)
  |> List.map ~f:(fun c -> String.of_char c)
;;

let part1 input =
  let lines = lines input in
  let numbers = List.map ~f:(fun line ->
    let number_row = extract_numbers line in
    (* Only keep the first and last numbers *)
    let first = List.hd_exn number_row in
    let last = List.last_exn number_row in
    first ^ last
  ) lines in
  let sum = List.map ~f:Int.of_string numbers |> List.fold ~init:0 ~f:(+) in
  sum
;;

let part2 input =
  -1
;;