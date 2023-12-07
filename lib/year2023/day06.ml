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

   (Time - Held Time) * Speed >= Record Distance
   (7-x)*x >= 9

  Assuming: 

 *)

type race_record = {
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

let parse_big_race input = 
  let lines = sp "\n" input in
  let time = hd_exn lines |> sp ":" |> last_exn |> sp " " |> filter_whitespace |> String.concat in
  let distance = last_exn lines |> sp ":" |> last_exn |> sp " " |> filter_whitespace |> String.concat in
  {
    time = int_of_string time;
    distance = int_of_string distance;
    speed = float_of_string distance /. float_of_string time;
  }

let rec calculate_charging_times record winning_times =
  let rec calculate_charging_times' record winning_times time =
    if record.time = time then 
      winning_times
    else if record.distance < (record.time - time) * time then
      calculate_charging_times' record (time :: winning_times) (time + 1)
    else
      calculate_charging_times' record winning_times (time + 1)
  in calculate_charging_times' record winning_times 1

let print_int_list_list = 
  iter ~f:(fun list -> 
    print_endline (String.concat ~sep:", " (map list ~f:string_of_int))
  )

let part1 input =
  let records = parse_races input in
  let winning_times = map records ~f:(fun record -> calculate_charging_times record []) in
  print_int (length winning_times);
  print_int (length (hd_exn winning_times));
  let count_winning_times = map winning_times ~f:(fun times -> length times) in
  let product = fold count_winning_times ~init:1 ~f:( * ) in
  print_endline (Printf.sprintf "Winning Times: %d" product);
  -1
;;

let part2 input =
  let big_record = parse_big_race input in
  let winning_times = calculate_charging_times big_record [] in
  length winning_times
;;
