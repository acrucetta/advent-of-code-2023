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

module Coord = struct
  type t = int * int [@@deriving equal, compare, sexp]
end

module IntMap = Map.Make (Int)
module CoordMap = Map.Make (Coord)

let parse input = input |> String.split_lines |> List.map ~f:String.to_list

let expand amount galaxy =
  let build_counts l =
    l
    |> List.mapi ~f:(fun i line ->
      i, if List.for_all line ~f:(Char.equal '.') then amount else 1)
    |> IntMap.of_alist_exn
  in
  build_counts (List.transpose_exn galaxy), build_counts galaxy
;;

let to_grid galaxy =
  galaxy
  |> List.concat_mapi ~f:(fun y line -> List.mapi line ~f:(fun x c -> (x, y), c))
  |> CoordMap.of_alist_exn
;;

let pairs grid =
  let rec all_pairs = function
    | a :: rest -> List.map rest ~f:(fun b -> a, b) @ all_pairs rest
    | [] -> []
  in
  grid |> Map.filter ~f:(Char.equal '#') |> Map.keys |> all_pairs
;;

let range a b = if a < b then List.range a b else List.range b a

let distance x_counts y_counts ((x1, y1), (x2, y2)) =
  let x_dist = range x1 x2 |> List.sum (module Int) ~f:(Map.find_exn x_counts) in
  let y_dist = range y1 y2 |> List.sum (module Int) ~f:(Map.find_exn y_counts) in
  x_dist + y_dist
;;

let solve amount input =
  let galaxy = parse input in
  let x_counts, y_counts = expand amount galaxy in
  galaxy
  |> to_grid
  |> pairs
  |> List.map ~f:(distance x_counts y_counts)
  |> Core.List.sum (module Int) ~f:Fn.id
;;

let part1 input = solve 2 input
let part2 input = solve 1000000 input