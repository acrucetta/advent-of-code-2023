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
  -1
;;