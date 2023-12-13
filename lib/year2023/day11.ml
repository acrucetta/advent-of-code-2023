open List
module P = Util.Parser
open P.Syntax

(* Day 11: Cosmic Expansion 

  We have a set of galaxies that are expanding. We want to calculate the 
  shortest path between each pair of galaxies. Then sum the length of all
  of the shortest paths.

    ....#.....
    .......#..
    #.........
    ..........
    ......#...
    .#........
    .........#
    ..........
    .......#..
    #...#.....

  According to the puzzle, it helps to give each galaxy a name. We will
  build a grid and assign an ID to each of the # characters.

  The catch is, the rows or columns that contain no galaxies have expanded.
  They will be twice as big. Increasing the distance between each of the galaxies.

  Tasks:
    - Parse the input and create a grid
    - Assign to each # character a unique id
    - Expand the grid for those rows with no galaxies
    - Use this grid to calculate the distance from each ID
      to the other set of IDs. We will only take the shortest
      path.

  Data Structures:
    - Map of Coord
    - Map of Ints?
*)

let sp ~on = Str.split (Str.regexp on)

module Coord = struct
  type t = int * int [@@deriving show, sexp, compare, equal]
end

module CoordMap = Map.Make(Coord)
module IntMap = Map.Make(Int)

let manhattan (x1,y1) (x2,y2) =
  abs (x1 - x2) + abs (y1 - y2)

let init_galaxy input =
  input
  |> String.split_lines
  |> List.map ~f:String.to_list
;;

let is_row_empty row =
  List.for_all ~f:(fun c -> c = '.') row
;;

(* 
  Build a map with id as the row and the value as the amount to expand.
  We set it as 2 by default. 
 *)
let expand init_galaxy amount =
  let get_counts row =
    row
    |> List.concat_mapi ~f:(fun i row ->
        i, if is_row_empty row then amount else 1)
    |> IntMap.of_alist_exn
  in
  get_counts (List.transpose_exn init_galaxy), get_counts init_galaxy
;;

let build_grid init_galaxy =
    init_galaxy
    |> List.concat_mapi ~f:(fun row_i row ->
      row
    |> List.mapi ~f:(fun col_i col ->
      (row_i, col_i), col))
    |> CoordMap.of_alist_exn
;;

(* Get X and Y coordinates for planets within the galaxy
  We have a map of (x,y) and we need to filter the values
  within that map for only the pairs with # then we need
  to remove the duplicates in those pairs
 *)
let get_pairs galaxy =
  galaxy
    |> Map.filter ~f:(Char.equal '#')
;;

let part1 input =
  let galaxy = init_galaxy input in
  let row_counts, col_counts = expand galaxy 2 in
  row_counts, col_counts
;;

let part2 input =
  -1
;;
