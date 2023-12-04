
(*
Day 4: Scratchards
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11

We want to find a match between the right side and the left side. For each match we will double
the score per card.

Approach:
- Split each card by the | 
- Store each set of numbers in a list
- Add each match from the first half into a list of matches
- Based on the length the score will be 2^length-1
- Return the fold left sum of all the lengths as output
 *)
open List

let sp = String.split_on_chars

let cards input =
  input
  |> sp ~on:['\n']
  |> map ~f:(fun card ->
      let card = sp ~on:[':'] card in
      List.last_exn card
      )

let part1 input =
  let cards = cards input in
  print_endline (List.hd_exn cards);
;;

let part2 input =
  -1
;;
