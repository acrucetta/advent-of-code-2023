
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

let sp = String.split_on_chars

let cards input =
  input
  |> sp ~on:['\n']
  |> List.map ~f:(fun card ->
      let card = sp ~on:[':'] card in
      List.last_exn card
      )
      |> List.filter ~f:(fun x -> not (String.is_empty x))


(*
  [get_matches] will iterate over each number in [numbers]
  and find a match in [winners] if there's a match we keep
  the number if not, we filter it out. We will return an int
  representing the score for the card.
*)
let get_powerscore winners numbers =
  let matches = List.filter ~f:(List.mem ~equal:String.equal winners) numbers in
  let length_matches = (List.length matches) in
  if length_matches = 0 then 0
  else Z.pow (Z.of_int 2) (length_matches - 1) |> Z.to_int
;;

let get_countscore winners numbers =
  let matches = List.filter ~f:(List.mem ~equal:String.equal winners) numbers in
  let length_matches = (List.length matches) in
  if length_matches = 0 then 0
  else length_matches
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
  let winners = sides |> List.hd_exn |> sp ~on:[' '] |> List.filter ~f:(fun x -> not (is_whitespace x)) in
  let numbers = sides |> List.last_exn |> sp ~on:[' '] |> List.filter ~f:(fun x -> not (is_whitespace x)) in
  (winners, numbers)
;;

let part1 input =
  let cards = cards input in
  cards
    |> List.map ~f:(fun card ->
        let (winners, numbers) = get_cards card in
        get_powerscore winners numbers
      )
    |> List.fold_left ~init:0 ~f:(+)
;;

let cascade_counts (arr : int array) (matches : int list) = 
  let len = Array.length arr in
  for idx = 0 to len - 1 do
    if idx < List.length matches then
      let match_count = List.nth_exn matches idx in
      for i = 1 to match_count do 
        let next_idx = idx + i in
        if next_idx < len then
          arr.(next_idx) <- arr.(next_idx) + arr.(idx)
      done
  done
;;

let print_int_array arr =
  Array.iter arr ~f:(fun x -> print_int x; print_string " ");
  print_string "\n"

let print_list_int arr =
  List.iter arr ~f:(fun x -> print_int x; print_string " ");
  print_string "\n"

let print_list_string arr =
  List.iter arr ~f:(fun x -> print_string x; print_string "\n");
;;

let part2 input =
  let cards = cards input in
  let matches = List.map ~f:(fun card ->
      let (winners, numbers) = get_cards card in
      let score = get_countscore winners numbers in
      score
      ) cards in
      let max_matches = List.length cards in
      let scratchcard_counts = Array.create ~len:max_matches 1 in
      cascade_counts scratchcard_counts matches;
      Array.sum (module Int) scratchcard_counts ~f:(fun x -> x)
;;
