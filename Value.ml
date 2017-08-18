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
| In_channel of BatIO.input
| Out_channel of unit BatIO.output

let nil = Sumtype ("()", None)
let true_val = Sumtype ("true", None)
let false_val = Sumtype ("false", None)

let stdin_chan = In_channel stdin
let stdout_chan = Out_channel stdout
let stderr_chan = Out_channel stderr

let to_int = function Int i -> i | _ -> raise TypeError
let to_float = function Float f -> f | _ -> raise TypeError
let to_char = function Char c -> c | _ -> raise TypeError
let to_string = function String s -> s | _ -> raise TypeError
let to_array = function Array a -> a | _ -> raise TypeError
let to_tuple = function Tuple t -> t | _ -> raise TypeError
let to_function = function Function f -> f | _ -> raise TypeError
let to_variant = function Variant (s, v) -> (s, v) | _ -> raise TypeError
let to_sumtype = function Sumtype (s,v) -> (s, v) | _ -> raise TypeError
let to_record = function Record r -> r | _ -> raise TypeError
let to_module = function Module m -> m | _ -> raise TypeError
let to_functor = function Functor f -> f | _ -> raise TypeError
let to_input = function In_channel c -> c | _ -> raise TypeError
let to_output = function Out_channel c -> c | _ -> raise TypeError

let to_bool = function
| Sumtype ("true", None) -> true
| Sumtype ("false", None) -> false
| _ -> raise TypeError

let from_int i = Int i
let from_float f = Float f
let from_char c = Char c
let from_string s = String s
let from_array a = Array a
let from_tuple t = Tuple t
let from_function f = Function f
let from_variant (s, v) = Variant (s, v)
let from_sumtype (s, v) = Sumtype (s, v)
let from_record r = Record r
let from_module m = Module m
let from_functor f = Functor f
let from_input i = In_channel i
let from_output o = Out_channel o

let from_bool = function
| true -> true_val
| false -> false_val
