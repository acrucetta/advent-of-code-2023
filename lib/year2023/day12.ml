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

type spring_state = Operational | Broken | Unknown [@@deriving compare, sexp_of, hash]

module CacheKey = struct 
    type t = spring_state list * int [@@deriving compare, sexp_of, hash]
end

type spring_row = {
  states: spring_state array;
  size: int list;
}

let char_to_spring_state = function
  | '.' -> Operational
  | '#' -> Broken
  | '?' -> Unknown
  | _ -> failwith "Invalid spring state"

let parse_spring_row line =
  let split_line = String.split_on_chars ~on:[' '] line in
  Printf.printf "Split line: %s\n" (List.sexp_of_t String.sexp_of_t split_line |> Sexp.to_string_hum);
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

let size spring sizes =
  let rec count_contiguous row num =
    match row with
    | [] -> num
    | Operational::tl -> num
    | Unknown::tl -> failwith "There shouldn't be unkowns in the arrangement"
    | Broken::tl -> count_contiguous tl (num + 1)
  in
  let rec iterate_spring spring sizes =
    match spring with
    | [] -> sizes
    | Unknown::tl -> failwith "There shouldn't be unkowns in the arrangement"
    | Operational::tl -> iterate_spring tl sizes
    | Broken::tl ->
      let num = count_contiguous tl 1 in
      iterate_spring tl (num::sizes)
  in
  let sizes = iterate_spring spring [] in
  sizes
;;

let is_valid_arrangement spring sizes =
  let calculated_sizes = size spring [] in
  List.equal Int.equal calculated_sizes sizes
;;

let rec permute cache row idx = 
  if idx = Array.length row.states then
    if is_valid_arrangement (Array.to_list row.states) row.size 
      then 1 
      else 0
  else
    match Hashtbl.find cache idx with
    | Some count -> count
    | None ->
      let count = match Array.get row.states idx with
      | Unknown -> 
        Array.set row.states idx Operational;
        let op_count = permute cache row (idx + 1) in
        Array.set row.states idx Broken;
        let broken_count = permute cache row (idx + 1) in
        Array.set row.states idx Unknown;
        op_count + broken_count
      | _ -> permute cache row (idx + 1)
      in 
      Printf.printf "Count: %d\n" count;
      Hashtbl.set cache ~key:idx ~data:count;
      count
;;


let part1 input =
  let sample = "??##?? 2,2" in
  let springs = springs sample in
  let first_row = nth_exn springs 0 in
  let cache = Hashtbl.create (module Int) in
  permute cache first_row 0
;;

let part2 input =
  -1
;;


 