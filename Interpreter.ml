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
  | Pexp_variant (name, exp_opt) ->
    let val_opt = match exp_opt with
      | None -> None
      | Some v -> Some (run_expression state ctx v) in
    Value.Variant (name, val_opt)
  | Pexp_construct (ident, exp_opt) ->
    let val_opt = match exp_opt with
    | None -> None
    | Some v -> Some (run_expression state ctx v) in
    let name = Longident.last ident.txt in
    Value.Sumtype (name, val_opt)
  | Pexp_record (fields, with_clause) ->
    let base = match with_clause with
    | None -> BatMap.empty
    | Some exp ->
      match run_expression state ctx exp with
      | Value.Record r -> r in
    let add_to_record r (ident, exp) =
      let name = Longident.last ident.txt in
      let value = run_expression state ctx exp in
      let idx = State.add state (State.Normal value) in
      BatMap.add name idx r in
    Value.Record (BatList.fold_left add_to_record base fields)
  | Pexp_field (exp, fieldname) ->
    let field = Longident.last fieldname.txt in
    begin
      match run_expression state ctx exp with
      | Record r -> value_of_alloc state @@ State.get state (BatMap.find field r)
      | _ -> raise Value.TypeError
    end
  | Pexp_setfield (r_exp, fieldname, exp) ->
    let field = Longident.last fieldname.txt in
    begin
      match run_expression state ctx r_exp with
      | Record r ->
        let value = run_expression state ctx exp in
        let idx = BatMap.find field r in
        State.set state idx (Normal value) ;
        Value.Sumtype ("()", None)
      | _ -> raise Value.TypeError
    end
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
  | Ppat_constant c when ValueUtils.value_eq (run_constant c) value -> ctx
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
  | Ppat_array patts ->
    begin
      match value with
      | Value.Array vals ->
        let val_list = BatArray.to_list vals in
        if BatList.length patts <> BatList.length val_list then
          raise MatchFailureException
        else
          BatList.fold_left2 (match_pattern state) ctx val_list patts
      | _ -> raise Value.TypeError
    end
  | Ppat_variant (name, param) ->
    begin
      match value with
      | Value.Variant (vname, vval) ->
        if name = vname then
          match (param, vval) with
          | None, None -> ctx
          | Some v, Some p -> match_pattern state ctx p v
          | _ -> raise Value.TypeError
        else
          raise MatchFailureException
      | _ -> raise Value.TypeError
    end
  | Ppat_construct (ident, param) ->
    begin
      match value with
      | Value.Sumtype (vname, vval) ->
        let name = BatString.join "." (Longident.flatten ident.txt) in
        if name = vname then
          match (param, vval) with
          | None, None -> ctx
          | Some v, Some p -> match_pattern state ctx p v
          | _ -> raise Value.TypeError
        else
          raise MatchFailureException
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
