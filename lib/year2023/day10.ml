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

let sp ~on = Str.split (Str.regexp on)

let print_2d_array arr =
  Array.iteri
    ~f:(fun i row ->
      Array.iteri
        ~f:(fun j col ->
          Printf.printf "%s" col)
        row;
      Printf.printf "\n")
    arr
;;

let build_grid input =
  let rows = sp ~on:"\n" input in
  let matrix = map ~f:(fun row -> sp ~on:"" row) rows in
  Array.of_list (map ~f:(fun row -> Array.of_list row) matrix)
;;

let part1 input =
  let maze = build_grid input in
  print_2d_array maze;
  -1
;;

let part2 input =
  -1
;;
