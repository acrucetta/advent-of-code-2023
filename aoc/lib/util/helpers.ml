open Core

let extract_int_option opt = 
  match opt with
    | Some x -> x
    | None -> 0

(* Sums the ints in a list (lst) *)
let rec sum_ints lst =
  match lst with
    | [] -> 0
    | h::t -> h + (sum_ints t)

let max_int lst = List.max_elt lst ~compare:Int.compare

let top_n (n:int) (lst:int list) : int list =
  List.take (List.rev (List.sort lst ~compare:Int.compare)) n