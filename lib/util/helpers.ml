open Core

let extract_int_option opt = 
  match opt with
    | Some x -> x
    | None -> 0

let rec sum_ints lst =
  match lst with
    | [] -> 0
    | h::t -> h + (sum_ints t)

let max_int lst = Core.List.max_elt ~compare lst |> Option.value_exn

let top_n (n:int) (lst:int list) : int list =
  List.take (List.rev (List.sort lst ~compare:Int.compare)) n

let take l ~n = Core.List.take l n
let drop l ~n = Core.List.drop l n

let string_to_arr (str : string) : char array = 
  Array.of_list (List.init (String.length str) (String.get str))

(* Print list as [a;b;c;d] *)
let print_list (lst : 'a list) : unit =
  Printf.printf "[ ";
  List.iter lst ~f:(fun x -> Printf.printf "%s;" x);
  Printf.printf "]\n"

let rec print_hash (hash : (string, int) Hashtbl.t) : unit =
  Hashtbl.iteri hash ~f:(fun ~key ~data ->
      Printf.printf "%s: %d\n" key data)

