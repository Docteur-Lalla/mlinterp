open Batteries
open Parsetree
open Longident
open Asttypes

exception NotSupportedException of string
exception MatchFailureException
exception NotFunctionException of Value.t
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
        let ctx' = Context.add id idx ctx in
        State.set state idx @@ State.Prealloc (binding.pvb_expr, ctx') ;
        ctx'
      else
        let value = run_expression state ctx binding.pvb_expr in
        match_pattern state ctx value binding.pvb_pat in
    let ctx' = BatList.fold_left prealloc ctx bindings in
    run_expression state ctx' expr
  | Pexp_tuple exprs -> Value.Tuple (BatList.map (run_expression state ctx) exprs)
  | Pexp_array exprs ->
    let lst = BatList.map (run_expression state ctx) exprs in
    Value.Array (BatArray.of_list lst)
  | Pexp_function cases ->
    let func value = match_many_pattern state ctx value cases in
    Value.Function func
  | Pexp_fun (Nolabel, _, patt, expr) ->
    let func value = match_pattern state ctx value patt
      |> flip (run_expression state) expr in
    Value.Function func
  | Pexp_fun (Labelled _, _, _, _)
  | Pexp_fun (Optional _, _, _, _) -> raise @@ NotSupportedException "Labelled and optional argument"
  | Pexp_apply (fexp, arg_exps) ->
    let fval = run_expression state ctx fexp in
    let args = BatList.map snd arg_exps
      |> BatList.map (run_expression state ctx) in
    let rec apply_func fval = function
    | [] -> fval
    | arg :: rest ->
      begin
        match fval with
        | Value.Function func -> apply_func (func arg) rest
        | _ -> raise @@ NotFunctionException fval
      end in
    apply_func fval args
  | Pexp_match (expr, cases) ->
    let func = { exp with pexp_desc = Pexp_function cases } in
    let app = { exp with pexp_desc = Pexp_apply (func, [(Nolabel, expr)]) } in
    run_expression state ctx app
  | _ -> raise NotImplemented

(** This function matches the given value with the pattern and returns a context with
 * the variables defined with the pattern. *)
and match_pattern state ctx value patt =
  match patt.ppat_desc with
  | Ppat_any -> ctx
  | Ppat_var l ->
    let id = l.txt in
    let idx = State.add state (State.Normal value) in
    Context.add id idx ctx
  | Ppat_constant c when Value.value_eq (run_constant c) value -> ctx
  | Ppat_constant _ -> raise MatchFailureException
  | Ppat_alias (p, l) ->
    let ctx' = match_pattern state ctx value p in
    match_pattern state ctx' value { patt with ppat_desc = Ppat_var l }
  | Ppat_or (p1, p2) ->
    begin
      try match_pattern state ctx value p1
      with _ -> match_pattern state ctx value p2
    end
  | Ppat_tuple patts ->
    begin
      match value with
      | Value.Tuple vals ->
        if BatList.length patts <> BatList.length vals then
          raise MatchFailureException
        else
          BatList.fold_left2 (match_pattern state) ctx vals patts
      | _ -> raise Value.TypeError
    end
  | _ -> raise NotImplemented

and match_many_pattern state ctx value = function
| [] -> raise MatchFailureException
| case :: rest ->
  try
    let ctx' = match_pattern state ctx value case.pc_lhs in
    run_expression state ctx' case.pc_rhs
  with
  | MatchFailureException -> match_many_pattern state ctx value rest

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
