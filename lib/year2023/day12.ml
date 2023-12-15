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

type spring_state = Operational | Broken | Unknown [@@deriving compare, sexp, hash]

module Cache = struct 
    type t = spring_state list * int [@@deriving compare, sexp, hash]
end

module MemoCache = Hashtbl.Make( Cache )

type spring_row = {
  states: spring_state array;
  size: int list;
  mutable arrangements: int;
}
[@@deriving compare, sexp]

let char_to_spring_state = function
  | '.' -> Operational
  | '#' -> Broken
  | '?' -> Unknown
  | _ -> failwith "Invalid spring state"

let parse_spring_row line =
  let split_line = String.split_on_chars ~on:[' '] line in
  let states = List.map ~f:char_to_spring_state (nth_exn split_line 0 |> String.to_list) |> Array.of_list in
  let size = List.map ~f:Int.of_string (nth_exn split_line 1 |> String.split_on_chars ~on:[',']) in 
  { states; size ; arrangements = 0 }

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

let rec permute cache row idx arrangements =
  let key = (Array.to_list row.states, idx) in
  match Map.find_exn cache key with
  | Some arrangements -> arrangements
  | None ->
    if idx = Array.length row.states then 
      let sizes = size (Array.to_list row.states) [] in
      if List.equal Int.equal sizes row.size then
        permute cache row 0 (arrangements + 1)
    else
      match row.states.(idx) with
      | _ -> permute cache row (idx + 1)
      | Unknown ->
          row.states.(idx) <- Operational;
          let operational = permute cache row (idx + 1) arrangements in
          row.states.(idx) <- Broken;
          let broken = permute cache row (idx + 1) arrangements in
          operational + broken
;;


let part1 input =
  let sample_input = "???.### 1,1,3" in
  let row = parse_spring_row sample_input in
  let arrangements = permute (MemoCache.create ()) row 0 in
  Printf.printf "Arrangements: %d\n" arrangements;
  row.arrangements;
;;

let part2 input =
  -1
;;


 