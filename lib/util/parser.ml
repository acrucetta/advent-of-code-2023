module P = struct
  open Angstrom

  let is_whitespace = function ' ' | '\n' | '\t' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_char = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  let number = take_while1 is_digit >>| int_of_string
  let string = take_while1 is_char
end
