let input = {|
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
|}

let%expect_test "2023 day1 part1" =
  let result = Year2022.Day01.part1 input in
  Printf.printf "%i" result;
  [%expect {| 142 |}]
;;

let%expect_test "2023 day1 part2" =
  let result = Year2022.Day01.part2 input in
  Printf.printf "%i" result;
  [%expect {| 45000 |}]
;;

