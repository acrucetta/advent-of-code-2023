(*
The priorities go from a through z with 1 to 26. Then A through Z with 27 through 52.
 *)
open Base;;

let alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let priority_of_char c =
  match String.index alphabet c with
  | Some i -> i + 1
  | None -> failwith "Invalid character"
;;

let split_array (arr : char array) : char array * char array = 
  let len = Array.length arr in
  let mid = len / 2 in
  let left = Array.sub arr ~pos:0 ~len:mid in
  let right = Array.sub arr ~pos:mid ~len:(len - mid) in
  (left, right)
;;

(* Splits an input of strings into different lines
then splits each line into two parts, we end up
with a list [("seq1","seq2 of each line")] *)
let sequences (input : string) : (char array * char array) list =
  let lines = Str.split (Str.regexp "\n") input in 
  List.map lines (fun line -> 
    split_array (Util.Helpers.string_to_arr line)
  )
;;

let find_common_char (arr1 : char array) (arr2 : char array) : char option =
  let set1 = Set.of_array (module Char) arr1 in
  let set2 = Set.of_array (module Char) arr2 in
  Set.inter set1 set2 
  |> Set.to_list
  |> List.hd
;;

let part1 input = 
  let sequences = sequences input in
  let common_chars = List.map sequences (fun (arr1, arr2) -> 
    find_common_char arr1 arr2
  ) in
  let priorities = List.map common_chars (fun c -> 
    match c with
    | Some c -> priority_of_char c
    | None -> 0
  ) in
  List.fold priorities ~init:0 ~f:(+)
;;

let part2 input = 
  -1
;;

