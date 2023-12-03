include Angstrom

module Syntax = struct
  include Angstrom.Let_syntax

  let ( >>= ), ( >>| ), ( *> ), ( <* ), ( <|> ), ( <?> ), ( <$> ) =
    ( >>= ), ( >>| ), ( *> ), ( <* ), ( <|> ), ( <?> ), ( <$> )
  ;;

  let ( $> ) p a = p >>| const a
  let ( <$ ) a p = p >>| const a
end

let is_whitespace = function ' ' | '\n' | '\t' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_char = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let whitespace = take_while is_whitespace

let integer =
  let open Syntax in
  let%map tokens =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  Int.of_string tokens
;;

let parse_exn parser input = 
  input |> parse_string ~consume:Prefix parser |> Result.ok_or_failwith
;;