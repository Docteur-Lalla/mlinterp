open Batteries

exception TypeError ;;

(** Internal representation of OCaml values in this interpreter. *)
type t =
| Int of int
| Float of float
| Char of char
| String of string
| Array of t array
| Tuple of t list
| Function of (t -> t)

(** Convert a value to its textual representation. *)
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

let type_of_value = function
| Int _ -> "int"
| Float _ -> "float"
| Char _ -> "char"
| String _ -> "string"
| Array _ -> "array"
| Tuple _ -> "tuple"
| Function _ -> "function"

let print_value v =
  let ty = type_of_value v in
  print_endline @@ "val " ^ string_of_value v ^ " : " ^ ty

let rec value_eq a b = match (a, b) with
| (Int i1, Int i2) -> i1 = i2
| (Float f1, Float f2) -> f1 = f2
| (Char c1, Char c2) -> c1 = c2
| (String s1, String s2) -> s1 = s2
| (Array a1, Array a2) -> BatArray.for_all2 value_eq a1 a2
| (Tuple t1, Tuple t2) -> BatList.for_all2 value_eq t1 t2
| (Function _, Function _) -> false
| _ -> raise TypeError
