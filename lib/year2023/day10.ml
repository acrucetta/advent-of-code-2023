open List
module P = Util.Parser
open P.Syntax

(* Day 10: Pipe Maze

  The pipes are arranged in a two-dimensional grid of tiles:

| is a vertical pipe connecting north and south.
- is a horizontal pipe connecting east and west.
L is a 90-degree bend connecting north and east.
J is a 90-degree bend connecting north and west.
7 is a 90-degree bend connecting south and west.
F is a 90-degree bend connecting south and east.
. is ground; there is no pipe in this tile.
S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
Based on the acoustics of the animal's scurrying, you're confident the pipe that contains the animal is one large, continuous loop.

Based on the initial starting position; we want to find the tile in the loop that is the farthest from the starting position.  We need to find the tile that would take the longest number of steps along the loop.

.....
.S-7.
.|.|.
.L-J.
.....

In the loop above, we start at S and try to find the max position in that loop from the starting position which is J.

*)

module Coord = struct
  type t = int * int [@@deriving show, sexp, compare, equal]
end

module CoordMap = Map.Make(Coord)
module CoordSet = Set.Make(Coord)

let sp ~on = Str.split (Str.regexp on)

type direction = N | S | E | W [@@deriving show, eq, ord, sexp]

let move (x, y) = function
  | N -> (x, y-1)
  | S -> (x, y+1)
  | E -> (x+1, y)
  | W -> (x-1, y)

let build_grid input =
  input
  |> String.split_lines
  |> List.concat_mapi ~f:(fun y line ->
    line |> String.to_list |> List.mapi ~f:(fun x c -> (x, y), c))
  |> CoordMap.of_alist_exn
;;

let neighbors maze (x,y) = 
  [(N, (x, y-1)); (S, (x, y+1)); (E, (x+1, y)); (W, (x-1, y))]
  |> List.filter_map ~f:(fun (d, c) -> Map.find maze c |> Option.map ~f:(fun c -> (d, c)))
;;

let initial_direction maze start =
  start 
  |> neighbors maze
  |> List.find_map ~f:(function
    | N, v when List.mem ['|'; '7'; 'F'] v ~equal:Char.equal -> Some (N, v)
    | S, v when List.mem ['-'; 'J'; '7'] v ~equal:Char.equal -> Some (S, v)
    | E, v when List.mem ['|'; 'J'; 'L'] v ~equal:Char.equal -> Some (E, v)
    | W, v when List.mem ['-'; 'F'; 'L'] v ~equal:Char.equal -> Some (W, v)
    | _ -> None)
  |> Option.value_exn

let next_dir maze d coord =
  let c = Map.find_exn maze coord in
  match d, c with
  | N, '|' -> N
  | N, 'F' -> E
  | N, '7' -> W
  | S, '|' -> S
  | S, 'J' -> W
  | S, 'L' -> E
  | W, 'F' -> S
  | W, '-' -> W
  | W, 'L' -> N
  | E, '-' -> E
  | E, 'J' -> N
  | E, '7' -> S
  | _ -> failwith "Invalid direction"

let traverse maze start dir =
  let rec loop path coord dir = 
    let coord' = move coord dir in
    let path' = Set.add path coord' in
    if Coord.equal coord' start then path'
    else (
      let dir' = next_dir maze dir coord' in
      loop path' coord' dir'
    )
  in
  loop CoordSet.empty start dir

let find_start maze =
  maze
  |> Map.to_alist
  |> List.find_map ~f:(fun (k, c) -> if Char.equal 'S' c then Some k else None)
  |> Option.value_exn
  |> fun (x,y) -> (x,y)
;;

let print_valid_neighbors neighbors = 
  iter ~f:(fun (d, c) -> Printf.printf "%s char: %s \n" (show_direction d) (Char.to_string c)) neighbors
;;

let print_maze maze = 
  maze 
  |> Map.to_alist
  |> List.iter ~f:(fun (k, c) -> Printf.printf "%s - value: %s \n" (Coord.show k) (Char.to_string c))

let part1 input =
  let maze = build_grid input in
  let start = find_start maze in
  let directions, coordinates = initial_direction maze start in
  let path = traverse maze start directions in 
  let path_length = Set.length path in
  path_length / 2
;;

let part2 input =
  -1
;;
