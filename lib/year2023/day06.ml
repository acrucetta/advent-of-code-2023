open List

let sp on = Str.split (Str.regexp on) 

(* 
  Day 6: Boat Race

  The sample input is:
    Time:      7  15   30
    Distance:  9  40  200

  We have a toyboat that speeds up for each millisecond we press it. Our 
  goal is to beat each column. Where 7 is the length of the race and 9 
  is the millimeters the boat traversed during that time.

  Assuming we can press the button for part of that time, for what 
  ranges of time can we press the boat speed to beat each competitor.

  Finaly, we will grab that range of times for each race and multiply it together.

  E.g., 288 (4*8*9)

  Assuming: 

 *)

type opponent_stats = {
  time: int;
  distance: int;
  speed: float;
}
[@@deriving equal]

let print_string_list list = 
  print_endline (String.concat ~sep:", " list)
;;

let filter_whitespace list = 
  filter list ~f:(fun x -> String.length x > 0)
;;

let parse_races input = 
  let lines = sp "\n" input in
  let times = hd_exn lines |> sp ":" |> last_exn |> sp " " |> filter_whitespace in
  let distances = last_exn lines |> sp ":" |> last_exn |> sp " " |> filter_whitespace in
  let races = transpose_exn [times;distances] in
  map races ~f:(fun [t;d] -> 
    {
      time = int_of_string t;
      distance = int_of_string d;
      speed = float_of_string d /. float_of_string t;
}) 


let part1 input =
  let opponents = parse_races input in
  print_endline (Printf.sprintf "Opponents: %d" (length opponents));
  print_endline (Printf.sprintf "Sample Opponent -> Speed %f Distance %d" (hd_exn opponents).speed (hd_exn opponents).distance);
  -1
;;

let part2 input =
  -1
;;
