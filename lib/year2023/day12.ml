open List

(* 
   Day 12 Hot Springs

  Sprints are arranged into rows. For each row, the conidtion shows
  whether the spring is operational . or damaged #.

  Some springs we don't know so they're ?

  After the row of springs, we have a list of their size. This always
  accounts for every damaged springs. Groups are separated by operational springs.

  i.e., ##### is 4 never 2,2

  However, some of the springs conditions are unkown. So they're classified with a ?

  We want to figure out how many poossible arrangements of operational and broken strings
  fit the pattern.
 *)

type spring_state = Operational | Broken | Unknown [@@deriving show]

type spring_row = {
  states: spring_state array;
  size: int list;
}
[@@deriving show]

let char_to_spring_state = function
  | '.' -> Operational
  | '#' -> Broken
  | '?' -> Unknown
  | _ -> failwith "Invalid spring state"

let parse_spring_row line =
  let split_line = String.split_on_chars ~on:[' '] line in
  let states = List.map ~f:char_to_spring_state (nth_exn split_line 0 |> String.to_list) |> Array.of_list in
  let size = List.map ~f:Int.of_string (nth_exn split_line 1 |> String.split_on_chars ~on:[',']) in 
  { states; size }

let springs input =
  input
  |> String.split_on_chars ~on:['\n']
  |> map ~f:(fun line ->
      parse_spring_row line
    )
;;

(* 
  We want to recurse over each possible
  unkown and make it operational or broken
  then check if the arrangement is valid; if
  it's valid increment the count
 *)

let is_valid_arrangement spring =
  (* We want to compare the number of 
     contiguous # with the int size *)
  -1
;;

(* 
Count the arrangements; check if they're valid
and then add one to the final count of valid arrangements
for each row 
*)
let count_arrangements spring =
  -1
;;

let part1 input =
  let sample_input = "???.### 1,1,3" in
  let row = parse_spring_row sample_input in
  Printf.printf "%s\n" (show_spring_row row);
  -1
;;

let part2 input =
  -1
;;


