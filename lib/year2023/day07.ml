open List

(*
  Day 6: Camel Cards

  
  Today we're playing camel cards. We will get a list of hands and our goal
  is to order them based on the strength.

  A hand consists of five cards. Labeled: A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2

  Their strength follows this order where A is the highest and 2 is the lowest. 

  Every hand is exactly one type; from strongest to weakest.
  1 - 5 of a kind
  2 - 4 of a kind
  3 - full house (3 of a kind and 2 of a kind)
  4- 3 of a kind
  5 - 2 pair
  6 - 1 pair
  7 - high card

  Caveats:
    - If we have a tie, we compare the first card in each hand. If these cards are different
    the hand with the stronger first card wins. If they have the same we go to the next card.

  Our input is:
    32T3K 765
    T55J5 684
    KK677 28
    KTJJT 220
    QQQJA 483

  Each hand is followed by its bid amopunt. The winning amount is the rank 
  from weakest (1) to strongest (n-1) multiplied by the bid.

  We add up the winning bid and the multiplication with its rank.
 *)

let sp on = Str.split (Str.regexp on)

type card =
  | Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two | Joker
  [@@deriving compare, show]

let card_to_int = function
  | Ace -> 14
  | King -> 13
  | Queen -> 12
  | Jack -> 11
  | Ten -> 10
  | Nine -> 9
  | Eight -> 8
  | Seven -> 7
  | Six -> 6
  | Five -> 5
  | Four -> 4
  | Three -> 3
  | Two -> 2
  | Joker -> 1

type hand_type = 
  | FiveOfAKind 
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
  [@@deriving compare, enumerate, equal, show]

type round = {
  cards: card list;
  bid: int;
  hand_type: hand_type;
}

let hand_to_type hand =
  print_endline hand;
  let freq_map = String.fold hand ~init:(Map.empty (module Char)) ~f:(fun acc c ->
    match Map.find acc c with
    | Some v -> Map.set acc ~key:c ~data:(v + 1)
    | None -> Map.set acc ~key:c ~data:1
  ) in
  let freq_map = Map.data freq_map |> List.sort ~compare:Int.compare in
  match freq_map with
  | [1; 1; 1; 1; 1] -> HighCard
  | [2; 3] -> FullHouse
  | [1; 1; 1; 2] -> OnePair
  | [1; 2; 2] -> TwoPair
  | [1; 1; 3] -> ThreeOfAKind
  | [1; 4] -> FourOfAKind
  | [5] -> FiveOfAKind
  | _ -> failwith "Invalid hand"

let print_int_list lst = 
  List.map lst ~f:(fun i -> string_of_int i) |> String.concat ~sep:" "

let find_match_with_joker freq_values num_jokers =
  match freq_values with 
  | [1; 4] | [5] -> FiveOfAKind
  | [1;1;3] -> if num_jokers > 0 then FourOfAKind else ThreeOfAKind
  | [2;3] -> 
      begin match num_jokers with
      | 0 -> FullHouse
      | 1 -> FourOfAKind
      | 2 -> FiveOfAKind
      | _ -> failwith "Invalid hand"
      end
  | [1;2;2] -> 
      begin match num_jokers with
      | 0 -> TwoPair
      | 1 -> ThreeOfAKind
      | 2 -> FourOfAKind
      | _ -> failwith "Invalid hand"
      end
  | [1;1;1;2] -> 
      begin match num_jokers with
      | 0 -> OnePair
      | 1 -> TwoPair
      | 2 -> ThreeOfAKind
      | _ -> failwith "Invalid hand"
      end
  | [1;1;1;1;1] -> 
      begin match num_jokers with
      | 0 -> HighCard
      | 1 -> OnePair
      | _ -> failwith "Invalid hand"
      end
    | _ -> failwith "Invalid hand"


let hand_to_type' hand =
  let freq_map = String.fold hand ~init:(Map.empty (module Char)) ~f:(fun acc c ->
    match Map.find acc c with
    | Some v -> Map.set acc ~key:c ~data:(v + 1)
    | None -> Map.set acc ~key:c ~data:1
  ) in
  let num_jokers = Map.find freq_map 'J' |> Option.value ~default:0 in
  let freq_values = Map.data freq_map |> List.sort ~compare:Int.compare in
  find_match_with_joker freq_values num_jokers
;;

let hand_to_cards hand =
  let cards = String.to_list hand in
  let cards = List.map cards ~f:(fun c ->
    match c with
    | 'A' -> Ace
    | 'K' -> King
    | 'Q' -> Queen
    | 'J' -> Joker
    | 'T' -> Ten
    | '9' -> Nine
    | '8' -> Eight
    | '7' -> Seven
    | '6' -> Six
    | '5' -> Five
    | '4' -> Four
    | '3' -> Three
    | '2' -> Two
    | _ -> failwith "Invalid card"
  ) in
  cards

let parse_rounds input =
  let lines = sp "\n" input in
  let rounds = map ~f:(fun line ->
    let line = sp " " line in
    let hand = nth_exn line 0 in
    let bid = nth_exn line 1 |> Int.of_string in
    let cards = hand_to_cards hand in
    let hand_type = hand_to_type hand in
    {cards; bid; hand_type}
  ) lines in
  rounds
;;

let parse_rounds' input =
  let lines = sp "\n" input in
  let rounds = map ~f:(fun line ->
    let line = sp " " line in
    let hand = nth_exn line 0 in
    let bid = nth_exn line 1 |> Int.of_string in
    let cards = hand_to_cards hand in
    let hand_type = hand_to_type' hand in
    {cards; bid; hand_type}
  ) lines in
  rounds

let rec compare_cards_lists lst1 lst2 = 
  match (lst1 , lst2) with
  | ([], []) -> 0
  | ([], _) -> -1
  | (_, []) -> 1
  | (h1::t1, h2::t2) ->
      match compare_card h1 h2 with
      | 0 -> compare_cards_lists t1 t2
      | cmp -> cmp

let compare_round round1 round2 = 
  match compare_hand_type round1.hand_type round2.hand_type with
  | 0 ->
      compare_cards_lists (round1.cards) (round2.cards)
  | cmp -> cmp
;;

let print_round round = 
  let cards = List.map round.cards ~f:(fun c -> show_card c) |> String.concat ~sep:"" in
  Printf.printf "%s %d %s\n" cards round.bid (show_hand_type round.hand_type)

let part1 input =
  let rounds = parse_rounds input in
  let sorted_rounds = List.sort rounds ~compare:compare_round in
  let scores = List.mapi sorted_rounds ~f:(fun i round ->
    let rank = List.length sorted_rounds - i in
    rank * round.bid
  ) in
  let score = List.fold scores ~init:0 ~f:(+) in
  score
;;

let part2 input =
  let rounds = parse_rounds' input in
  let sorted_rounds = List.sort rounds ~compare:compare_round in
  let scores = List.mapi sorted_rounds ~f:(fun i round ->
    let rank = List.length sorted_rounds - i in
    print_round round;
    rank * round.bid
  ) in
  let score = List.fold scores ~init:0 ~f:(+) in
  score
;;
