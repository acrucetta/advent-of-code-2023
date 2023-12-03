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
 
 *)
open List
let sp = String.split_on_chars
let i = Fun.id

type grid = string list list

let build_grid input = 
  input 
  |> sp ~on:['\n']
  |> map ~f:(sp ~on:[' '])
;;

let part1 input =
  let sample_grid = "a b c\nd e f\ng h i" in
  build_grid sample_grid;
  print_endline (sample_grid);
  -1
;;

let part2 input =
  -1
;;
