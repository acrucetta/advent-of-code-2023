open List

type grid = char list list
type direction = Up | Down | Left | Right | UpLeft | UpRight | DownLeft | DownRight
type coord = int * int

let directions = [Up; Down; Left; Right; UpLeft; UpRight; DownLeft; DownRight]

let sep = String.split_on_chars

let build_grid input = 
  input 
  |> sep ~on:['\n']
  |> map ~f:(fun row -> row |> String.to_list |> List.of_list)
  
let get_directions = function
  | Up -> (0, -1) | Down -> (0, 1) | Left -> (-1, 0) | Right -> (1, 0)
  | UpLeft -> (-1, -1) | UpRight -> (1, -1) | DownLeft -> (-1, 1) | DownRight -> (1, 1)

let is_valid_coord (row, col) grid =
  row >= 0 && col >= 0 && row < length grid && col < length (hd grid)

let is_adjacent_symbol (row, col) grid =
  exists ~f:(fun dir ->
    let (dx, dy) = get_directions dir in
    let new_coord = (row + dy, col + dx) in
    is_valid_coord new_coord grid &&
    let cell = nth (nth grid (fst new_coord)) (snd new_coord) in
    mem cell ['*'; '#'; '$'; '+']
  ) directions

let rec extract_number grid row col acc =
  if not (is_valid_coord (row, col) grid) then (acc, col)
  else match nth (nth grid row) col with
    | '0'..'9' as digit -> extract_number grid row (col + 1) (acc ^ String.make 1 digit)
    | _ -> (acc, col)

let load_numbers grid =
  fold_left ~f:(fun acc row ->
    fold_left ~f:(fun acc col ->
      match nth (nth grid row) col with
      | '0'..'9' ->
        let (num, next_col) = extract_number grid row col "" in
        if num <> "" && is_adjacent_symbol (row, col) grid then
          (num :: acc, next_col)
        else
          (acc, next_col)
      | _ -> (acc, col + 1)
    ) acc (0 --^ (length (hd grid)))
  ) ([], 0) (0 --^ (length grid))
  |> fst

let part1 input =
  let grid = build_grid input in 
  let numbers = load_numbers grid in
  fold_left (fun acc num -> acc + int_of_string num) 0 numbers

let part2 input = -1
