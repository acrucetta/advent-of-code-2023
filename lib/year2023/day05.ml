open List

(*
Day 05: Mapping Seeds!

We have an input of type:
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4

We need to find the lowest location that corresponds to any of the initial seeds. The map for each row is:

  destination_start | source start | range_length

Range length describes how long the mapping for source will go. 

The mapping is represented as:

Seed Soil
0 0
1 1 
... ...

Each range gives us a set of possible matches between categories. We'd like to create this range by reading the mapping and representing it in a Set or a List.

We can use this mapping to find the lowest numbers that will take us from one map to the other. Ultimately, we want to find the lowest location as we go from map to map.


 *)

let sp on = Str.split (Str.regexp on) 
;;


type interval = {
  destination_start: int;
  source_start: int;
  length: int;
}

type map = {
  source: string;
  destination: string;
  ranges: interval list;
}

let seeds input = 
  let seed_line = hd_exn (sp "\n\n" input) in
  let seeds = List.last_exn (sp ":" seed_line) in
  let seed_list = sp " " seeds in
  List.map seed_list ~f:int_of_string
;;

let parse_maps input =
  let maps = sp "\n\n" input in
  let parse_map map = 
    let lines = sp "\n" map in
    let parse_line line = 
      let parts = sp " " line in
      let destination_start = List.nth_exn parts 0 in
      let source_start = List.nth_exn parts 1 in
      let length = List.nth_exn parts 2 in
      {destination_start = int_of_string destination_start;
       source_start = int_of_string source_start;
       length = int_of_string length}
    in
    List.map lines ~f:parse_line
  in
  List.map maps ~f:parse_map

let print_int_list l = 
  List.iter l ~f:(fun x -> print_int x; print_string " ");
  print_string "\n";
;;

let part1 input  = 
  let seed_list = seeds input in 
  seed_list;
  print_int_list seed_list;
  -1
;;

let part2 input = 
  -1
;;
