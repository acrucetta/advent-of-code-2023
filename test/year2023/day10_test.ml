let input = {|
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
|}

let%expect_test "2023 day10 part1" =
  Printf.printf "%i" @@ Year2023.Day10.part1 input;
  [%expect {|4|}]
;;

let%expect_test "2023 day10 part2" =
  Printf.printf "%i" @@ Year2023.Day10.part2 input;
  [%expect {|-1|}]
;;
