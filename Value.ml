open Batteries

type t =
| Int of int
| Float of float
| Char of char
| String of string
| Array of t array
| Tuple of t list
| Function of (t -> t)

let rec string_of_value = function
| Int i -> string_of_int i
| Float f -> string_of_float f
| Char c -> "'" ^ String.make 1 c ^ "'"
| String s -> "\"" ^ s ^ "\""
| Array [| |] -> "[| |]"
| Array ary ->
  let contents = BatArray.to_list ary
    |> BatList.map string_of_value
    |> BatString.concat " ; " in
  "[| " ^ contents ^ "|]"
| Tuple t ->
  let contents = BatList.map string_of_value t
    |> BatString.concat ", " in
  "(" ^ contents ^ ")"
| Function _ -> "<function>"

let print_value = print_endline % string_of_value
