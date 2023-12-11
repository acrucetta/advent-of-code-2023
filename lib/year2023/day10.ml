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
  type t = int * int [@@deriving show, eq, ord, sexp]
end

module CoordMap = Map.Make(Coord)
module CoordSet = Set.Make(Coord)

let sp ~on = Str.split (Str.regexp on)

type direction = N | S | E | W

let build_grid input =
  input
  |> String.split_lines
  |> List.concat_mapi ~f:(fun y line ->
    line |> String.to_list |> List.mapi ~f:(fun x c -> (x, y), c))
  |> CoordMap.of_alist_exn
;;

let find_start maze =
  maze
  |> Map.to_alist
  |> List.find_map ~f:(fun (k, c) -> if Char.equal 'S' c then Some k else None)
  |> Option.value_exn
;;

let part1 input =
  let maze = build_grid input in
  let start = find_start maze in
  start;
  -1
;;

let part2 input =
  -1
;;
