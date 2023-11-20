open Printf

(* Get the max sum of rows separated by a escape line '\n' and by 
   two \n\n *)
let totals input =
  input 
  |> Str.split (Str.regexp "\n\n")
  |> List.map ~f:(fun lst ->
      lst
      |> String.split_lines
      |> List.map ~f:Int.of_string 
      |> Util.Helpers.sum_ints)
  ;;

let part1 input =
  (* Print the list *)
  let totals = totals input in 
    (* Get the max of the totals *)
    match Util.Helpers.max_int totals with 
    | Some max -> max
    | None -> -1
;;

let part2 input =
  (*  Print the input *)
  print_endline input;

  (* Get top 3 *)
  let totals = totals input in 
  let top3 = Util.Helpers.top_n 3 totals in
  let sum_of_top_totals = List.sum (module Int) top3 ~f:Fn.id 
;;