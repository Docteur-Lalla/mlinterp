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
| Variant of string * t option
| Sumtype of string * t option
| Record of (string, int) BatMap.t
| Module of (string, int) BatMap.t
| Functor of (t -> t)

let nil = Sumtype ("()", None)
let true_val = Sumtype ("true", None)
let false_val = Sumtype ("false", None)
