(*
Creating a module to contain the intervals
- start and end
- contains
*)

type interval = {
  start: int;
  end_: int;
}

let is_contained int1 int2 =  
  int1.start <= int2.start && int1.end_ >= int2.end_

let split str delim =
  Str.split (Str.regexp delim) str

let parse_interval str =
  match split str "-" with
  | [start; end_] -> {start = int_of_string start; end_ = int_of_string end_}
  | _ -> failwith "Invalid input"

let parse_line line =
  match split line "," with
  | [int1_str; int2_str] -> (parse_interval int1_str, parse_interval int2_str)
  | _ -> failwith "Invalid input"
;;

let parse_multiline_input input = 
  let lines = split input "\n" in
  List.map lines ~f:parse_line
;;

(*
  This function will take a list of intervals, iterate through them, and
  count +1 for each pair of interval that contains each other.
 *)
let part1 input = 
  let interval_pairs = parse_multiline_input input in
  List.map interval_pairs ~f:(fun (int1, int2) -> 
    if is_contained int1 int2 || is_contained int2 int1 then 1 else 0
    )
  |> List.fold ~init:0 ~f:(+)
;;

let part2 input =
  -1
;;
