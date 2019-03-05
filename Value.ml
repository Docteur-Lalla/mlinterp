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
| In_channel of in_channel
| Out_channel of out_channel

let nil = Sumtype ("()", None)
let true_val = Sumtype ("true", None)
let false_val = Sumtype ("false", None)

let stdin_chan = In_channel Pervasives.stdin
let stdout_chan = Out_channel Pervasives.stdout
let stderr_chan = Out_channel Pervasives.stderr

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

let to_nil = function
| Sumtype ("()", None) -> ()
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

let from_nil () = nil
