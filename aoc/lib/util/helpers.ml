open Core

(* Sums the ints in a list (lst) *)
let rec sum_ints lst =
  match lst with
    | [] -> 0
    | h::t -> h + (sum_ints t)

let max_int lst = List.max_elt lst ~compare:Int.compare