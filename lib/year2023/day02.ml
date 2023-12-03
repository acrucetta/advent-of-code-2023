(*

Day 2

Sample input:
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

The elfs would like to know which games would have been possible if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes? In this case, only games 1, 2, and 5 would have been possible.

Goal: Determine which games would have been possible if the bag contained 12 red cubes, 13 green cubes, and 14 blue cubes.
 *)

(*
  Idea: 
    Build map of the color and the count with valid cubes
    Build map of each game and add to the map however many cubes we have
    in each game
    Check if each game is possible for each map (i.e., is game_cube_i < valid_cubes_i)
    If so, add the game id to a counter

  We will use the angstrom parser to parse the sample input for each line. We will load the values to a map of type: {"blue":12, "red":13, "green":14}
 *)

module P = Util.Parser
open Angstrom
include Angstrom.Let_syntax


type draw = {color: string; count: int}
type draws = draw list
type game = {id: int; round : draws list}

let color_p = P.choice [P.string "red"; P.string "green"; P.string "blue"]
let draw_p =
  let%map count = P.integer <* P.char ' ' and 
      color = color_p in 
      {color = color; count = count}

let draws_p = P.sep_by1 (P.string ", ") draw_p
let round_p = P.sep_by1 (P.string "; ") draws_p
let game_p =
  let%map id = P.string "Game " *> P.integer <* P.string ": " and
      round = round_p in
      {id = id; round = round}

let games_p = P.sep_by1 P.end_of_line game_p

let max_draws round =
  let table = Hashtbl.create (module String) in
  List.iter (List.concat round) ~f:(fun draw ->
    let count = Hashtbl.find table draw.color in
    match count with
    | Some c -> Hashtbl.set table ~key:draw.color ~data:(max c draw.count)
    | None -> Hashtbl.set table ~key:draw.color ~data:draw.count
  );
  table

let max_counts = [("green", 13); ("blue", 14); ("red", 12)]

(* 
  The game is possible if the count of each color is less than the max count
  If the game is possible, we return the id of the game, otherwise we return 0
*)
let is_game_possible game = 
  let table = Hashtbl.create (module String) in
  let max_draws = max_draws game.round in
  let is_possible = List.for_all max_counts ~f:(fun (color, count) ->
    let game_count = Hashtbl.find max_draws color in
    match game_count with
    | Some c -> c <= count
    | None -> false
  ) in
  if is_possible then game.id else 0
;;

(* Multiply each of the max number of cubes
   and return the power *)
let power_cubes max_draws = 
  let power = Hashtbl.fold max_draws ~init:1 ~f:(fun ~key:_ ~data:count acc ->
    acc * count
  ) in
  power

let part1 input = 
    input 
    |> P.parse_exn games_p
    |> List.map ~f:is_game_possible
    |> List.fold_left ~init:0 ~f:(+) 
;;

let part2 input = 
  let games = input |> P.parse_exn games_p in
  let powers = List.map games ~f:(fun game -> 
    let max_draws = max_draws game.round in
    let power = power_cubes max_draws in
    power
  ) in 
  List.fold_left powers ~init:0 ~f:( + )
;;
  
