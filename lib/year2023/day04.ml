
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


(*
  [get_matches] will iterate over each number in [numbers]
  and find a match in [winners] if there's a match we keep
  the number if not, we filter it out. We will return an int
  representing the score for the card.
*)
let get_score winners numbers =
  let matches = filter ~f:(fun x -> not (mem x winners)) numbers in  
  let length_matches = (length matches) in
  if length_matches = 0 then 0
  else Z.pow (Z.of_int 2) (length_matches - 1) |> Z.to_int
;;

let is_whitespace c = String.is_empty (String.strip c)

(* 
   [get_score] will load each string of [card]
   separate them by "|" and store each side
  into a list of numbers.

  Then it will iterate over the numbers on the left
  and find matches in the right side; if there's a match
  we keep the number if not, we filter it out
 *)
let get_cards card =
  let sides = sp ~on:['|'] card in
  let winners = sides |> List.hd_exn |> sp ~on:[' '] |> filter ~f:(fun x -> not (is_whitespace x)) in
  let numbers = sides |> List.last_exn |> sp ~on:[' '] |> filter ~f:(fun x -> not (is_whitespace x)) in
  (winners, numbers)
;;

let part1 input =
  let cards = cards input in
  cards
    |> map ~f:(fun card ->
        let (winners, numbers) = get_cards card in
        get_score winners numbers
      )
    |> fold_left ~init:0 ~f:(+)
;;

let part2 input =
  -1
;;
