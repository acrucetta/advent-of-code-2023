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

let directon_to_coord = function
  | N -> (0, -1)
  | S -> (0, 1)
  | E -> (1, 0)
  | W -> (-1, 0)

let next_coord (x,y) d = 
  let (dx, dy) = directon_to_coord d in
  (x + dx, y + dy)

let char_to_coordinates = function
  | '-' -> [E; W]
  | '|' -> [N; S]
  | 'L' -> [N; E]
  | 'J' -> [N; W]
  | '7' -> [S; W]
  | 'F' -> [S; E]
  | '.' -> []
  | _ -> []

let build_grid input =
  input
  |> String.split_lines
  |> List.concat_mapi ~f:(fun y line ->
    line |> String.to_list |> List.mapi ~f:(fun x c -> (x, y), c))
  |> CoordMap.of_alist_exn
;;

let neighbors maze (x,y) = 
  let directions = [(N, (x, y-1)); (S, (x, y+1)); (E, (x+1, y)); (W, (x-1, y))] in
  let valid_directions = List.filter ~f:(fun (_, (x,y)) -> Map.mem maze (x,y)) directions in
  let chars = valid_directions |> List.map ~f:(fun (d, (x,y)) -> (d, Map.find_exn maze (x,y) )) in
  let valid_chars = List.filter ~f:(fun (d,c) ->
    match (d,c) with
    | (N, '|') | (S, '|') 
    | (E, '-') | (W, '-')
    | (N, 'L') | (E, 'L')
    | (S, 'L')
    | (N, 'J') | (W, 'J')
    | (S ,'J')
    | (S, '7') | (W, '7')
    | (N, '7') | (E, '7')
    | (S, 'F') | (E, 'F') 
    | (N, 'F') -> true
    | _ -> false) chars in
  valid_chars
;;

let dfs_path (maze : char CoordMap.t) (start : int * int) =
  let rec loop visited current_node current_steps max_steps =
    let neighbors = neighbors maze current_node in
    let next_nodes = List.map ~f:(fun (d, c) -> next_coord current_node d) neighbors in
    let unvisited = List.filter ~f:(fun n -> not (Set.mem visited n)) next_nodes in
    match unvisited with
    | [] -> max max_steps current_steps
    | _ -> 
      List.fold_left unvisited ~init:max_steps ~f:(
        fun acc_max_steps next_node ->
          let visited = Set.add visited next_node in
          loop visited next_node (current_steps + 1) acc_max_steps)
  in
  loop (CoordSet.singleton start) start 0 0
;;

let bfs_path maze start =
  let queue = Queue.create () in
  Queue.enqueue queue start;
  let visited = CoordSet.singleton start in
  let rec loop queue visited current_steps max_steps =
    let curr_node = Queue.dequeue_exn queue in
    (* TODO: Solve when the coordinates are not in the bounds of the maze *)
    let neighbors = neighbors maze curr_node in
    let next_nodes = List.map ~f:(fun (d, c) -> next_coord curr_node d) neighbors in
    let unvisited = List.filter ~f:(fun n -> not (Set.mem visited n)) next_nodes in
    Queue.enqueue_all queue unvisited;
    match queue with
    | queue when Queue.is_empty queue -> max max_steps current_steps
    | _ -> 
      List.fold_left unvisited ~init:max_steps ~f:(
        fun acc_max_steps next_node ->
          let visited = Set.add visited next_node in
          loop queue visited (current_steps + 1) acc_max_steps)
  in
  loop queue visited 0 0

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
  Printf.printf "start: %s \n" (Coord.show start);
  let neighbors = neighbors maze start in
  let max_steps = bfs_path maze start in
  Printf.printf "max steps: %d \n"
  max_steps;
  -1
;;

let part2 input =
  -1
;;
