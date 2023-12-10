open List

(*

Day 8: Haunted Wasteland


We receive a list of instructions in the shape of:

RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)

We want to find the amount of steps it takes to go from AAA to ZZZ
given the instructions above (RL) Where L means visited the left node
and R the right node. 

We can loop all over again if we don't find it the first time. RL means 
RLRLRLRLRL...

We may need to use LCM to solve this problem. 

 *)

let sp ~on = Str.split (Str.regexp on)
module P = Util.Parser
open P.Syntax

module StrMap = Map.Make(String)

type direction = R | L 
type node = string * string
type graph = {
  directions: direction Sequence.t;
  map: node StrMap.t;
}

let dir_p = P.choice [ R <$ P.char 'R'; L <$ P.char 'L' ]
let dirs_p = P.many_till dir_p P.end_of_line >>| Sequence.cycle_list_exn

let token_p =
  P.take_while1 (function 'A' .. 'Z' -> true | _ -> false)

let pair_p =
  let%map left = P.char '(' *> token_p <* P.string ", "
  and right = token_p <* P.char ')' in
  left , right
;;

let entry_p = 
  let%map key = token_p <* P.string " = "
  and value = pair_p in
  key , value

let map_p = P.sep_by1 P.end_of_line entry_p >>| StrMap.of_alist_exn

let input_p = 
  let%map directions = dirs_p <* P.end_of_line
  and map = map_p in
  { directions; map }
;;

let part1 input =
  let graph = P.parse_exn input_p input in
  let map = graph.map in 
  let dirs = graph.directions in
  let traverse_graph (steps, node) dir = 
    if String.equal node "ZZZ"
    then Error steps
    else ( 
      let left,right = Map.find_exn map node in
      match dir with 
      | L -> Ok (steps + 1, left)
      | R -> Ok (steps + 1, right))
  in 
  dirs
  |> Sequence.fold_result ~init:(0,"AAA") ~f:traverse_graph
  |> Result.error
  |> Option.value_exn;
  -1
;;

let part2 input =
  let graph = P.parse_exn input_p input in
  let is_start str = String.is_suffix str ~suffix:"A" in
  let is_end str = String.is_suffix str ~suffix:"Z" in
  let map = graph.map in 
  let dirs = graph.directions in
  let all_starts = Map.fold map ~init:[] ~f:(fun ~key ~data acc ->
      if is_start key then key :: acc else acc) in
  let traverse_graph (steps, node) dir = 
    if is_end node
    then Error steps
    else ( 
      let left,right = Map.find_exn map node in
      match dir with 
      | L -> Ok (steps + 1, left)
      | R -> Ok (steps + 1, right))
  in 
  let counts_steps = List.map ~f:(fun start -> 
      dirs
      |> Sequence.fold_result ~init:(0,start) ~f:traverse_graph
      |> Result.error
      |> Option.value_exn) all_starts
  in
  counts_steps;
  -1
;;
