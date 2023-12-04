(*

Day 3

We receive an engine schematic:
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

And we need to find the parts that are adjacent to a symbol. In the case above 114 and 58 are not adjacent to the parts. Therefore, we can't count them in our sum.

Approach:
  - We need to parse the input into some form of graph.
  - Whenever there's a number, we should look around that number to see if there's a symbol,the number can be a span of multiple digits. 
  - We will load the numbers that have symbols such as ( *, #, $)
  - We will return the sum of all these numbers

Cases:
  - First number has symbol around it; returns true; capture all the next numbers after it; accumulate
  - Middle number has symbol around it; capture numbers left and right of the number; accumulate
  - Last number has symbol around it; capture numbers left of that number; accumulate
 
 *)
module P = Util.Parser
open List
let sp = String.split_on_chars
let i = Fun.id

type grid = string list list

type direction = 
  | Up
  | Down
  | Left
  | Right
  | UpLeft
  | UpRight
  | DownLeft
  | DownRight

let get_directions = function
  | Up -> (0, -1)
  | Down -> (0, 1)
  | Left -> (-1, 0)
  | Right -> (1, 0)
  | UpLeft -> (-1, -1)
  | UpRight -> (1, -1)
  | DownLeft -> (-1, 1)
  | DownRight -> (1, 1)

let directions = [Up; Down; Left; Right; UpLeft; UpRight; DownLeft; DownRight]

let build_grid input = 
  input 
  |> sp ~on:['\n']
  |> map ~f:(fun row -> List.of_list (String.to_list row))
;;

let get_cell grid (row, col) =
  let max_row = length grid in
  let max_col = length (nth_exn grid 0) in
  if row < 0 || row >= max_row || col < 0 || col >= max_col then None
  else
    let row = nth_exn grid row in
    let cell = nth_exn row col in
    Some cell 

let is_digit c = function '0'..'9' -> true | _ -> false

let is_by_symbol grid (row, col) =
  let max_row = length grid in
  let max_col = length (nth_exn grid 0) in
  let validate_coords (row, col) = row >= 0 && row < max_row && col >= 0 && col < max_col in
  let rec is_by_symbol' (row, col) = function
    | [] -> false
    | direction :: directions ->
      let (dx, dy) = get_directions direction in
      let new_row = row + dy in
      let new_col = col + dx in
      if validate_coords (new_row, new_col) then
        match get_cell grid (new_row, new_col) with
        | Some cell when mem cell ['*'; '#'; '$';'+'] -> true
        | _ -> is_by_symbol' (row, col) directions
      else
        is_by_symbol' (row, col) directions
  in
  is_by_symbol' (row, col) directions
;;

let extract_number grid row col =
  let rec extract col current_number current_coords =
    match get_cell grid (row,col) with
    | Some cell when is_digit cell ->
      extract (col + 1) (current_number ^ String.make 1 cell) ((row, col) :: current_coords)
    | _ -> (current_number, current_coords)
  in
  extract col "" []

  let load_numbers grid =
    let rec extract_number row col =
      let rec extract col current_number current_coords =
        match get_cell grid (row, col) with
        | Some cell when is_digit cell ->
          extract (col + 1) (current_number ^ String.make 1 cell) ((row, col) :: current_coords)
        | _ -> (current_number, List.rev current_coords)  (* Reverse for correct order *)
      in
      extract col "" []
    in
    let rec traverse row col numbers =
      if row >= length grid then numbers
      else
        let next_row, next_col =
          if col >= length (nth_exn grid row) - 1 then row + 1, 0
          else row, col + 1
        in
        match get_cell grid (row, col) with
        | Some cell when is_digit cell ->
          let (num, coords) = extract_number row col in
          let new_numbers = (num, coords) :: numbers in
          traverse next_row (col + length coords) new_numbers
        | _ -> traverse next_row next_col numbers
    in
    traverse 0 0 []

let part1 input =
  let grid = build_grid input in 
  let numbers = load_numbers grid in
  let rec sum_numbers = function
    | [] -> 0
    | (num, coords) :: numbers ->
      let (row, col) = nth_exn coords 0 in
      if is_by_symbol grid (row, col) then
        Int.of_string num + sum_numbers numbers
      else
        sum_numbers numbers
  in
  sum_numbers numbers
;;

let part2 input =
  -1
;;
