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

type cards = 
  | A 
  | K 
  | Q
  | J
  | T
  | N of int

let card_to_int = function
  | A -> 14
  | K -> 13
  | Q -> 12
  | J -> 11
  | T -> 10
  | N n -> n

type hand = cards list

type round = {
  hand: hand;
  bid: int;
}

type hand_type = 
  | FiveOfAKind 
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard


let part1 input =
  -1
;;

let part2 input =
  -1
;;
