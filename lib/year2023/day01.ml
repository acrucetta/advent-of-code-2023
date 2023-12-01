

let split_into_lines input =
  input
  |> Str.split (Str.regexp "\n")
;;

let print_list lst =
  List.iter lst ~f:(fun item ->
    print_endline (match item with
    | Some s -> s
    | None -> ""
    )
  )
;;

let to_digit str = 
  match str with
  | "one" -> "1"
  | "two" -> "2"
  | "three" -> "3"
  | "four" -> "4"
  | "five" -> "5"
  | "six" -> "6"
  | "seven" -> "7"
  | "eight" -> "8"
  | "nine" -> "9"
  | _ -> str

let extract_numbers line =
  line
  |> String.to_list
  |> List.filter ~f:(fun c -> Char.is_digit c)
  |> List.map ~f:(fun c -> String.of_char c)
;;

(* 
We want to search for a digit or a number in the line.
We will need to use a regexp for that. The 
two patterns will be [0-9] and [one|two|three|...]

We will grab all the matches to either pattern and put them in a list.
Then will use the list to convert the spelled out numbers to digits.
*)
let extract_digits_and_numbers line =
  let re = Str.regexp "\\(one\\|two\\|three\\|four\\|five\\|six\\|seven\\|eight\\|nine\\|[0-9]\\)" in
  let regex_matches = Str.full_split re line in
  let matches = List.map ~f:(fun match_ ->
    match match_ with
    | Str.Delim s -> Some (to_digit s)
    | Str.Text _ -> None
  ) regex_matches in 
  List.filter_opt matches
;;

  let part1 input =
  let lines = split_into_lines input in
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
  let lines = split_into_lines input in
  let numbers = List.map ~f:(fun line ->
    let number_row = extract_digits_and_numbers line in
    (* Only keep the first and last numbers *)
    if List.length number_row = 0 then
      "0"
    else if List.length number_row = 1 then
      List.hd_exn number_row
    else
      let first = List.hd_exn number_row in
      let last = List.last_exn number_row in
      first ^ last
  ) lines in
  Util.Helpers.print_list numbers;
  let sum = List.map ~f:Int.of_string numbers |> List.fold ~init:0 ~f:(+) in
  sum
;;