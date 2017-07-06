open Batteries
open Parsetree

(** This function interprets a constant literal and returns the corresponding value. *)
let run_constant = function
| Pconst_integer (s, _) -> Value.Int (int_of_string s)
| Pconst_float (s, _) -> Value.Float (float_of_string s)
| Pconst_char c -> Value.Char c
| Pconst_string (s, _) -> Value.String s

(** This function interprets an expression and returns the computed value. *)
let rec run_expression state ctx exp =
  match exp.pexp_desc with
  | Pexp_constant cst -> run_constant cst
  | _ -> Value.Int 0

let rec run_structure_item state ctx item =
  match item.pstr_desc with
  | Pstr_eval (exp, _) -> (ctx, run_expression state ctx exp)
  | _ -> (ctx, Value.Int 0)

let rec run_structure state ctx =
  let run_item (ctx, _) = run_structure_item state ctx in
  BatList.fold_left run_item (ctx, Value.Int 0)
