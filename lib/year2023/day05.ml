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
  destination_end: int;
  source_start: int;
  source_end: int;
  length: int;
}

type seed_range = {
  start_: int;
  end_: int;
}

let single_seeds input = 
  let seed_line = hd_exn (sp "\n\n" input) in
  let seeds = List.last_exn (sp ":" seed_line) in
  let seed_list = sp " " seeds in
  List.map seed_list ~f:int_of_string
;;

let pair_seeds input =
  let seed_line = hd_exn (sp "\n\n" input) in
  let seeds = List.last_exn (sp ":" seed_line) in
  let seed_list = sp " " seeds in
  let rec pair_seeds seed_list =
    match seed_list with
    | [] -> []
    | [_] -> []
    | a::b::rest -> (a,b)::(pair_seeds rest)
  in
  pair_seeds seed_list

let explode_seed_pairs seed_pairs = 
  map seed_pairs ~f:(fun (seed,length) ->
    let seed = Int.of_string seed in
    let length = Int.of_string length in
    {
      start_ = seed;
      end_ = seed + length - 1;
    })
;;

let parse_maps input =
  let maps = tl_exn (sp "\n\n" input) in
  let parse_map map = 
    let lines = tl_exn (sp "\n" map) in 
    let parse_line line = 
      let parts = sp " " line in
      let length = Int.of_string (List.nth_exn parts 2) in
      let destination_start = Int.of_string (List.nth_exn parts 0) in
      let destination_end = destination_start + length - 1 in
      let source_start = Int.of_string (List.nth_exn parts 1) in
      let source_end = source_start + length -1 in
      {
        destination_start;
        destination_end;
        source_start;
        source_end;
        length;
      }
    in
    List.map lines ~f:parse_line
  in
  List.map maps ~f:parse_map

  let rec find_in_map_ranges seed_range map =
  match map with
  | [] -> seed_range
  | range::other_ranges -> 
      if seed_range.start_ <= range.source_end &&
      seed_range.end_ >= range.source_start then
        let overlap = {
          start_ = max seed_range.start_ range.source_start;
          end_ = min seed_range.end_ range.source_end;
        } in
        let offset = range.destination_start - range.source_start in
        let new_range = {
          start_ = overlap.start_ + offset;
          end_ = overlap.end_ + offset;
        } in
        print_endline (Printf.sprintf "Seed range %d-%d matched to the source range %d-%d, offset %d, resulting in %d-%d" seed_range.start_ seed_range.end_ range.source_start range.source_end offset new_range.start_ new_range.end_);
        new_range
      else
        let () = print_endline (Printf.sprintf "Seed range %d-%d did not match to the source range %d-%d" seed_range.start_ seed_range.end_ range.source_start range.source_end) in
        find_in_map_ranges seed_range other_ranges

let rec find_in_map seed map =
  match map with
  | [] -> seed
  | range::other_ranges -> 
      if seed >= range.source_start && seed <= range.source_end then
        seed + (range.destination_start - range.source_start)
    else 
      find_in_map seed other_ranges

let rec find_location_range seed_range maps =
  match maps with
  | [] -> seed_range
  | map::other_maps -> 
    let locations = find_in_map_ranges seed_range map in
    find_location_range locations other_maps


let rec find_location_seed seed maps =
  match maps with
  | [] -> seed
  | map::other_maps -> 
    let location = find_in_map seed map in
    find_location_seed location other_maps
;;

let print_int_list l = 
  List.iter l ~f:(fun x -> print_int x; print_string " ");
  print_string "\n";
;;

let print_interval i = 
  print_string (Printf.sprintf "%d-%d -> %d-%d\n" i.source_start i.source_end i.destination_start i.destination_end)
;;

let part1 input  = 
  let seed_list = single_seeds input in 
  let maps = parse_maps input in
  let locations = map seed_list ~f:(fun seed -> find_location_seed seed maps) in
  List.min_elt locations ~compare:Int.compare |> Option.value_exn
;;

let part2 input = 
  let seed_pairs = pair_seeds input in
  let expanded_seeds = explode_seed_pairs seed_pairs in
  let maps = parse_maps input in
  let locations = map expanded_seeds ~f:(fun seed_range -> find_location_range seed_range maps) in
  List.min_elt locations ~compare:(fun a b -> Int.compare a.start_ b.start_) |> Option.value_exn |> (fun x -> x.start_)
;;
