open Batteries
open Parsetree
open Longident
open Asttypes

exception NotSupportedException of string
exception NotImplemented

(** This function interprets a constant literal and returns the corresponding value. *)
let run_constant = function
| Pconst_integer (s, _) -> Value.Int (int_of_string s)
| Pconst_float (s, _) -> Value.Float (float_of_string s)
| Pconst_char c -> Value.Char c
| Pconst_string (s, _) -> Value.String s

(** This function retrieves the value behind the given identifier. *)
let rec run_identifier state ctx = function
| Lident id -> Context.find id ctx
  |> State.get state
  |> value_of_alloc state
| Ldot (path, id) -> (* module *)
  Value.Int 0
| Lapply _ -> raise @@ NotSupportedException "Lapply identifier"

(** This function interprets an expression and returns the computed value. *)
and run_expression state ctx exp =
  match exp.pexp_desc with
  | Pexp_constant cst -> run_constant cst
  | Pexp_ident l ->
    let id = l.txt in
    run_identifier state ctx id
  | Pexp_let (rec_flag, bindings, expr) ->
    let prealloc ctx binding =
      if rec_flag = Recursive then
        let alloc = State.Prealloc (binding.pvb_expr, ctx) in
        let id = match binding.pvb_pat.ppat_desc with
        | Ppat_var l -> l.txt
        | _ -> raise @@ NotSupportedException "Non-variable pattern in recursive let-binding" in
        let idx = State.add state alloc in
        Context.add id idx ctx
      else
        let value = run_expression state ctx binding.pvb_expr in
        match_pattern state ctx value binding.pvb_pat in
    let ctx' = BatList.fold_left prealloc ctx bindings in
    run_expression state ctx' expr
  | _ -> raise NotImplemented

(** This function matches the given value with the pattern and returns a context with
 * the variables defined with the pattern. *)
and match_pattern state ctx value patt =
  match patt.ppat_desc with
  | Ppat_var l ->
    let id = l.txt in
    let idx = State.add state (State.Normal value) in
    Context.add id idx ctx
  | _ -> raise NotImplemented

(** This function executes a toplevel phrase. *)
and run_structure_item state ctx item =
  match item.pstr_desc with
  | Pstr_eval (exp, _) -> (ctx, run_expression state ctx exp)
  | _ -> raise NotImplemented

(** This function evaluates a module structure. *)
and run_structure state ctx =
  let run_item (ctx, _) = run_structure_item state ctx in
  BatList.fold_left run_item (ctx, Value.Int 0)

(** This function computes a value from its allocation in the state. *)
and value_of_alloc state = function
| State.Normal v -> v
| State.Prealloc (exp, ctx) -> run_expression state ctx exp
