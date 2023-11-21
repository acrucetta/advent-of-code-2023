open Core

let user_move_to_int (move : string) : int  =
  match move with
  | "A" -> 1 (* Rock *)
  | "B" -> 2 (* Paper *)
  | "C" -> 3 (* Scissors *)
  | _ -> -1

type outcome = Win | Lose | Tie

let outcome_to_value (outcome : outcome) : int =
  match outcome with
  | Win -> 6
  | Lose -> 0
  | Tie -> 3

let get_result (user_move : string) (opponent_move : string) : outcome =
  match (user_move, opponent_move) with
  | ("A", "X") -> Tie
  | ("A", "Y") -> Lose
  | ("A", "Z") -> Win
  | ("B", "X") -> Win
  | ("B", "Y") -> Tie 
  | ("B", "Z") -> Lose
  | ("C", "X") -> Lose
  | ("C", "Y") -> Win
  | ("C", "Z") -> Lose
  | _ -> Tie

let rounds input = 
  input
  |> Str.split (Str.regexp "\n")
  |> List.map ~f:(Str.split (Str.regexp " "))
;;

let part1 input =
  let parsed_rounds = rounds input in
  let rec play_rounds parsed_rounds score =
    match parsed_rounds with
    | [] -> score
    | round :: rest ->
      let user_move = List.nth_exn round 0 in
      let opponent_move = List.nth_exn round 1 in
      (* print_endline (user_move ^ " " ^ opponent_move);  *)
      let outcome = get_result user_move opponent_move in
      play_rounds rest (score + outcome_to_value outcome + (user_move_to_int user_move))
  in
  play_rounds parsed_rounds 0
;;

let part2 input =
  -1
;;