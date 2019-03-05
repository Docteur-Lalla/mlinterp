(*
 * Copyright 2019 Kévin Le Bon <kevin.le-bon@inria.fr>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *)

open Batteries
open Value

exception RuntimeException

(** Convert a value to its textual representation. *)
let rec string_of_value state = function
| Int i -> string_of_int i
| Float f -> string_of_float f
| Char c -> "'" ^ String.make 1 c ^ "'"
| String s -> "\"" ^ s ^ "\""
| Array [| |] -> "[| |]"
| Array ary ->
  let contents = BatArray.to_list ary
    |> BatList.map (string_of_value state)
    |> BatString.concat " ; " in
  "[| " ^ contents ^ " |]"
| Tuple t ->
  let contents = BatList.map (string_of_value state) t
    |> BatString.concat ", " in
  "(" ^ contents ^ ")"
| Function _ -> "<function>"
| Variant (name, None) -> "`" ^ name
| Variant (name, Some v) -> "`" ^ name ^ " " ^ string_of_value state v
| Sumtype (name, None) -> name
| Sumtype (name, Some v) -> name ^ " " ^ string_of_value state v
| Record r ->
  let string_of_field (name, idx) =
    let value = match State.get state idx with
    | State.Normal value -> value
    | State.Prealloc _ -> Sumtype ("...", None) in
    name ^ " = " ^ string_of_value state value in
  let fields_s = BatList.of_enum @@ BatEnum.map string_of_field (BatMap.enum r) in
  "{" ^ BatString.join " ; " fields_s ^ "}"
| Module m -> string_of_value state (Record m)
| Functor _ -> "<functor>"
| In_channel _ -> "<in_channel>"
| Out_channel _ -> "<out_channel>"

(** Get the string representation of the given value's type. *)
let rec type_of_value = function
| Int _ -> "int"
| Float _ -> "float"
| Char _ -> "char"
| String _ -> "string"
| Array _ -> "array"
| Tuple _ -> "tuple"
| Function _ -> "function"
| Variant (name, None) -> "[> `" ^ name ^ "]"
| Variant (name, Some v) -> "[> `" ^ name ^ " of " ^ type_of_value v ^ " ]"
| Sumtype _ -> "sumtype"
| Record _ -> "record"
| Module _ -> "module"
| Functor _ -> "functor"
| In_channel _ -> "in_channel"
| Out_channel _ -> "out_channel"

(** Print the value in stdout. *)
let print_value state v =
  let ty = type_of_value v in
  print_endline @@ "val " ^ string_of_value state v ^ " : " ^ ty

(** Equality check between two Value.t. *)
let rec value_eq a b = match (a, b) with
| (Int i1, Int i2) -> i1 = i2
| (Float f1, Float f2) -> f1 = f2
| (Char c1, Char c2) -> c1 = c2
| (String s1, String s2) -> s1 = s2
| (Array a1, Array a2) -> BatArray.for_all2 value_eq a1 a2
| (Tuple t1, Tuple t2) -> BatList.for_all2 value_eq t1 t2
| (Function _, Function _) -> false
| (Variant (n1, None), Variant (n2, None)) -> n1 = n2
| (Variant (n1, Some v1), Variant (n2, Some v2)) -> n1 = n2 && value_eq v1 v2
| (Variant _, Variant _) -> false
| (Sumtype (n1, None), Sumtype (n2, None)) -> n1 = n2
| (Sumtype (n1, Some v1), Sumtype (n2, Some v2)) -> n1 = n2 && value_eq v1 v2
| (Sumtype _, Sumtype _) -> false
| (Record r1, Record r2) -> BatMap.equal (=) r1 r2
| (Module m1, Module m2) -> BatMap.equal (=) m1 m2
| (Functor _, Functor _) -> false
| (In_channel c1, In_channel c2) -> c1 = c2
| (Out_channel c1, Out_channel c2) -> c1 = c2
| _ -> raise TypeError

let value_lt a b = match (a, b) with
| (Int i1, Int i2) -> i1 < i2
| (Float f1, Float f2) -> f1 < f2
| (Char c1, Char c2) -> c1 < c2
| (In_channel c1, In_channel c2) -> c1 < c2
| (Out_channel c1, Out_channel c2) -> c1 < c2
| _ -> raise TypeError
