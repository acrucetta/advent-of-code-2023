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

type color = Blue | Red | Green
type counts = {color: string; count: int}

let color_of_string = function
  | "blue" -> Blue
  | "red" -> Red
  | "green" -> Green
  | _ -> failwith "Invalid color"

(* Parser for a number *)
let number = take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

(* Parser for a color *)
let color = take_while1 (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false)

(* [color_count] parses "3 blue" and returns {color = Blue; count = 3} *)
let color_count =
  lift2 (fun count color -> ({color = color; count})) number (char ' ' *> color)

(* [group] parses "3 blue, 4 red" and returns [{color = Blue; count = 3}; {color = Red; count = 4}] *)
let group = sep_by (char ',') color_count <* skip_many (char ' ')

(* [line_parser] parses "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" and 
   returns [[{color = Blue; count = 3}; {color = Red; count = 4}]; 
   [{color = Red; count = 1}; {color = Green; count = 2};
    {color = Blue; count = 6}]; [{color = Green; count = 2}]] *)
let line_parser = sep_by (char ';') group <* skip_many (char ' ')

let aggregate_max_counts groups =
  let table = Hashtbl.create (module String) in
  List.iter ~f:(fun group ->
      List.iter ~f:(fun {color; count} ->
          let current_count = Hashtbl.find table color |> Option.value ~default:0 in
          Hashtbl.set table ~key:color ~data:(max current_count count)
        ) group
    ) groups;
  table

let parse_line line =
  match parse_string ~consume:All line_parser line with
  | Ok groups -> aggregate_max_counts groups
  | Error msg -> failwith msg

let part1 input = 
  let result = parse_line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" in
  Hashtbl.iteri result ~f:(fun ~key ~data ->
      Printf.printf "%s: %d\n" key data
    );
    -1
;;

let part2 input = -1;;
