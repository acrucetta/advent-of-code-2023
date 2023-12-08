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
  | Ace | King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two
  [@@deriving compare]

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

type hand_type = 
  | FiveOfAKind 
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
  [@@deriving compare, enumerate, equal]

type round = {
  hand: card list;
  bid: int;
  hand_type: hand_type;
}

(* [hand_to_type hand] returns the type [hand_type] of the hand.

  To do this, we need to parse each distinct string in the hand and count how many
  cards of the same type we have. Then we match them to the hand_type. If there are
  no matches, we have a high card.

  To do this, we will create a frequency map with each string.
 *)

let hand_to_type hand =
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

let parse_rounds input =
  let lines = sp "\n" input in
  let rounds = map ~f:(fun line ->
    let line = sp " " line in
    let hand = nth_exn line 0 in
    let bid = nth_exn line 1 |> Int.of_string in
    let hand_type = hand_to_type hand in
    { hand; bid; hand_type }
  ) lines in
  rounds
;;

let part1 input =
  -1
;;

let part2 input =
  -1
;;
