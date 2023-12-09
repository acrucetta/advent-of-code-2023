let input = {|
3LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
|}

let%expect_test "2023 day8 part1" =
  Printf.printf "%i" @@ Year2023.Day08.part1 input;
  [%expect {|6|}]
;;

let%expect_test "2023 day8 part2" =
  Printf.printf "%i" @@ Year2023.Day08.part2 input;
  [%expect {|-1|}]
;;
