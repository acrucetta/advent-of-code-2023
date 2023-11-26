(*
The input comes in the shape:The input comes in the shape:

    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2

We need to parse the moves and then execute them on the stacks.
We can create a separate stack of string and apply operations to them
in the order we're receiving them.

We can use a map to store each stack and then apply the operations to them.
 *)

 (* We want to parse each line of boxes 
Their shape is 
[S] [S] [S]
 1   2   3

We can use regex to parse each line to a list of strings. We want
to keep track of their order so we can use a map to store them.
i.e., which stack they're on. 
 *)   

let stack_map = Map.empty (module String);;

let parse_crate_line line =
  let crate_pattern = Str.regexp "\\[\\([A-Z]\\)\\]" in
  let rec extract_crates accu pos =
    try
      let _ = Str.search_forward crate_pattern line pos in
      let crate = Str.matched_group 1 line in
      extract_crates (crate :: accu) (match_end ())
    with Not_found -> accu
  in
  extract_crates [] 0
;;

let parse_crates input =
  (* Take the first input *)
  let crates = List.hd_exn (Str.split (Str.regexp "\n\n") input) in
  print_endline crates;
  let raw_crate_lines = Str.split (Str.regexp "\n") crates in 
  let crate_lines = List.map raw_crate_lines ~f:parse_crate_line in
     print_endline (List.hd_exn (List.hd_exn crate_lines));
    
;;

let part1 input = 
  parse_crates input;
  ""
;;

let part2 input =
  parse_crates input;
  "abc"
;;
