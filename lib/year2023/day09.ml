open List
module P = Util.Parser
open P.Syntax

(* Day 09: Mirage Maintenance

  0 3 6 9 12 15
  1 3 6 10 15 21
  10 13 16 21 30 45

  Each line contains the history of a single value. 
  We want to predict the next value in each history. To do this
  we make a new sequence from the difference at each step of the history.
  If the sequence is not all zeroes repeat the process. We take the difference
  from each step until they're all zeroes.
*)

let sp on = Str.split (Str.regexp on)

let get_histories input =
  let lines = sp "\n" input in
  let histories = map ~f:(fun l -> map ~f:int_of_string (sp " " l)) lines in
  histories
;;

(*
  For each history we want to calculate the differences between each step. 
  That is we will create a new recursive list with the differences between two
  consecutive numbers. We will do this until the list is all zeroes.

  We need to keep track of each list of differences. We will return the sum
  of the last number in each list.
 *)
let rec calculate_differences history acc =
    match history with
    | [] -> acc
    | [x] -> List.rev acc
    | x::y::xs -> calculate_differences (y::xs) ((y - x)::acc)
;;

let rec calculate_all_differences history acc =
  let differences = calculate_differences history [] in
  match differences with
  | [] -> acc
  | [0] -> acc
  | _ -> calculate_all_differences differences (last_exn [differences]::acc)
;;

let rec predict_last_value all_diffs acc =
  match all_diffs with
  | [] -> acc
  | x::xs -> predict_last_value xs (acc + last_exn x)

let print_int_list_list = 
  List.iter ~f:(fun l -> List.iter ~f:(Printf.printf "%d ") l; Printf.printf "\n")
;;

let part1 input =
  let histories = get_histories input in
  let predictions = map ~f:(fun h -> predict_last_value (calculate_all_differences h [h]) 0) histories in
  let prediction_sum = List.fold_left ~f:(+) ~init:0 predictions in
  prediction_sum
;;

let part2 input =
  -1
;;
