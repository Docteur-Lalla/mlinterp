open Batteries
open Parsetree
open Longident
open Asttypes

exception NotSupportedException of string
exception MatchFailureException
exception NotFunctionException of Value.t
exception NotImplemented
exception AssertFalseException
exception ExceptionRaised of Value.t

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
  begin
    match run_identifier state ctx path with
    | Value.Module md ->
      let idx = BatMap.find id md in
      let alloc = State.get state idx in
      value_of_alloc state alloc
    | _ -> raise Value.TypeError
  end
| Lapply _ -> raise @@ NotSupportedException "Lapply identifier"

(** This function interprets an expression and returns the computed value. *)
and run_expression state ctx exp =
  match exp.pexp_desc with
  | Pexp_constant cst -> run_constant cst

  | Pexp_ident l ->
    let id = l.txt in
    run_identifier state ctx id

  | Pexp_let (rec_flag, bindings, expr) ->
    let ctx' = run_let_binding state ctx rec_flag bindings in
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
    (** Check if the given constructor is a module name or an actual constructor. *)
    begin
      try
        let idx = Context.find (Longident.last ident.txt) ctx in
        match value_of_alloc state (State.get state idx) with
        | Value.Module _ as md -> md
        | _ -> raise Value.TypeError
      with
      | Not_found -> Value.Sumtype (name, val_opt)
    end

  | Pexp_record (fields, with_clause) ->
    let base = match with_clause with
    | None -> BatMap.empty
    | Some exp ->
      match run_expression state ctx exp with
      | Value.Record r -> r
      | _ -> raise Value.TypeError in
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
      | Value.Record r -> value_of_alloc state @@ State.get state (BatMap.find field r)
      | _ -> raise Value.TypeError
    end

  | Pexp_setfield (r_exp, fieldname, exp) ->
    let field = Longident.last fieldname.txt in
    begin
      match run_expression state ctx r_exp with
      | Value.Record r ->
        let value = run_expression state ctx exp in
        let idx = BatMap.find field r in
        State.set state idx (State.Normal value) ;
        Value.Sumtype ("()", None)
      | _ -> raise Value.TypeError
    end

  | Pexp_sequence (exp1, exp2) ->
    ignore @@ run_expression state ctx exp1 ;
    run_expression state ctx exp2

  | Pexp_while (cond, expr) ->
    while run_expression state ctx cond = Value.Sumtype ("true", None) do
      ignore @@ run_expression state ctx expr
    done ;
    Value.Sumtype ("()", None)

  | Pexp_for (patt, start, stop, dir, body) ->
    let value = ref (run_expression state ctx start) in
    let for_iter ctx value patt =
      let ctx' = match_pattern state ctx value patt in
      ignore @@ run_expression state ctx' body in
    let iter = function
    | Value.Int i ->
      begin
        match dir with
        | Upto -> Value.Int (i + 1)
        | Downto -> Value.Int (i - 1)
      end
    | _ -> raise Value.TypeError in
    let stop_value = match run_expression state ctx stop with
    | Value.Int i -> Value.Int (i + 1)
    | _ -> raise Value.TypeError in

    while ValueUtils.value_eq !value stop_value = false do
      for_iter ctx !value patt ;
      value := iter !value
    done ;
    Value.Sumtype ("()", None)

  | Pexp_ifthenelse (cond, exp1, exp2_opt) ->
    let cond_value = run_expression state ctx cond in
    if cond_value = Value.Sumtype ("true", None) then
      run_expression state ctx exp1
    else
      begin
        match exp2_opt with
        | Some exp2 -> run_expression state ctx exp2
        | None -> Value.Sumtype ("()", None)
      end

  | Pexp_assert exp ->
    let value = run_expression state ctx exp in
    if value = Value.Sumtype ("false", None) then
      raise AssertFalseException
    else
      Value.Sumtype ("()", None)

  | Pexp_try (exp, cases) ->
    begin
      try run_expression state ctx exp
      with
      | ExceptionRaised exc ->
        try match_many_pattern state ctx exc cases
        with MatchFailureException -> raise @@ ExceptionRaised exc
    end

  | Pexp_constraint (e, _)
  | Pexp_coerce (e, _, _)
  | Pexp_newtype (_, e)
  | Pexp_letexception (_, e) -> run_expression state ctx e

  | Pexp_pack mexp -> run_module_expression state ctx mexp

  | Pexp_open (_, ident, exp) ->
    begin
      match run_identifier state ctx ident.txt with
      | Value.Module md ->
        let ctx' = Context.open_module md ctx in
        run_expression state ctx' exp
      | _ -> raise Value.TypeError
    end

  | Pexp_letmodule (ident, mexp, exp) ->
    let id = ident.txt in
    let md = run_module_expression state ctx mexp in
    let idx = State.add state (State.Normal md) in
    let ctx' = Context.add id idx ctx in
    run_expression state ctx' exp

  | Pexp_poly _
  | Pexp_extension _
  | Pexp_new _
  | Pexp_lazy _
  | Pexp_send _
  | Pexp_object _
  | Pexp_override _
  | Pexp_setinstvar _ -> raise @@ NotSupportedException "Lazy, extension and object-related expressions"

  | Pexp_unreachable -> raise @@ NotSupportedException "Unreachable expressions"

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

  | Ppat_interval (c1, c2) ->
    let cst1 = run_constant c1 in
    let cst2 = run_constant c2 in
    begin
      match (value, cst1, cst2) with
      | Value.Int vi, Value.Int i1, Value.Int i2 ->
        if i1 <= vi && vi <= i2 then ctx else raise MatchFailureException
      | Value.Float vf, Value.Float f1, Value.Float f2 ->
        if f1 <= vf && vf <= f2 then ctx else raise MatchFailureException
      | Value.Char vc, Value.Char c1, Value.Char c2 ->
        if c1 <= vc && vc <= c2 then ctx else raise MatchFailureException
      | _ -> raise Value.TypeError
    end

  | Ppat_constraint _
  | Ppat_type _ -> ctx

  | Ppat_extension _
  | Ppat_lazy _ -> raise @@ NotSupportedException "Extension and lazy pattern"

  | _ -> raise NotImplemented

(** This function takes a case list and try them one after the other until one matches.
 * At this point, it runs the associated expression. *)
and match_many_pattern state ctx value = function
| [] -> raise MatchFailureException
| case :: rest ->
  try
    let ctx' = match_pattern state ctx value case.pc_lhs in
    match case.pc_guard with
    | None -> run_expression state ctx' case.pc_rhs
    | Some guard ->
      if run_expression state ctx' guard = Value.Sumtype ("true", None) then
        run_expression state ctx' case.pc_rhs
      else
        raise MatchFailureException
  with
  | MatchFailureException -> match_many_pattern state ctx value rest

(** This function evaluates every given let expression and stores the resulting bindings
 * in the context given in parameter. It returns the new context. *)
and run_let_binding state ctx rec_flag bindings =
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
  BatList.fold_left prealloc ctx bindings

and run_module_expression state ctx mod_expr =
  match mod_expr.pmod_desc with
  | Pmod_ident id -> run_identifier state ctx id.txt

  | Pmod_structure str ->
    let (ctx', _) = run_structure state ctx str in
    Value.Module (Context.to_map ctx')

  | Pmod_functor (id, _, expr) ->
    let func md =
      let idx = State.add state (State.Normal md) in
      let ctx' = Context.add id.txt idx ctx in
      run_module_expression state ctx' expr in
    Value.Functor func

  | Pmod_apply (fexp, mexp) ->
    let md = run_module_expression state ctx mexp in
    begin
      match run_module_expression state ctx fexp with
      | Value.Functor f -> f md
      | _ -> raise Value.TypeError
    end

  | Pmod_unpack exp ->
    begin
      match run_expression state ctx exp with
      | Value.Module _ as md -> md
      | _ -> raise Value.TypeError
    end

  | Pmod_constraint (mexp, _) -> run_module_expression state ctx mexp

  | Pmod_extension _ -> raise @@ NotSupportedException "Extensions"

(** This function executes a toplevel phrase. *)
and run_structure_item state ctx item =
  match item.pstr_desc with
  | Pstr_eval (exp, _) -> (ctx, run_expression state ctx exp)

  | Pstr_value (rec_flag, bindings) ->
    let ctx' = run_let_binding state ctx rec_flag bindings in
    (ctx', Value.Sumtype ("()", None))

  | Pstr_module m ->
    let md = run_module_expression state ctx m.pmb_expr in
    let idx = State.add state (State.Normal md) in
    let ctx' = Context.add m.pmb_name.txt idx ctx in
    (ctx', md)

  | Pstr_recmodule bindings ->
    let prealloc ctx binding =
      let exp = { pexp_desc = Pexp_pack binding.pmb_expr ; pexp_loc = Location.none ; pexp_attributes = [] } in
      let alloc = State.Prealloc (exp, ctx) in
      let idx = State.add state alloc in
      Context.add binding.pmb_name.txt idx ctx in
    let ctx' = BatList.fold_left prealloc ctx bindings in
    let indices = BatList.map (fun b -> Context.find b.pmb_name.txt ctx') bindings in
    let func ctx idx =
      let alloc = match State.get state idx with
      | State.Normal _ as n -> n
      | State.Prealloc (exp, ctx') -> State.Prealloc (exp, ctx) in
      State.set state idx alloc in
    let _ = BatList.iter (func ctx') indices in
    (ctx', Value.Sumtype ("()", None))

  | Pstr_open op ->
    begin
      match run_identifier state ctx op.popen_lid.txt with
      | Value.Module md -> (Context.open_module md ctx, Value.Sumtype ("()", None))
      | _ -> raise Value.TypeError
    end

  | Pstr_include inc ->
    begin
      match run_module_expression state ctx inc.pincl_mod with
      | Value.Module md ->
        let md_ctx = Context.from_map md in
        let ctx' = Context.include_context md_ctx ctx in
        (ctx', Value.Sumtype ("()", None))
      | _ -> raise Value.TypeError
    end

  | Pstr_type _
  | Pstr_typext _
  | Pstr_modtype _
  | Pstr_primitive _
  | Pstr_exception _ -> (ctx, Value.Sumtype ("()", None))

  | Pstr_attribute _
  | Pstr_extension _ -> raise @@ NotSupportedException "Extensions and attributes"

  | Pstr_class _
  | Pstr_class_type _ -> raise @@ NotSupportedException "Class related statements"
    
  (* | _ -> raise NotImplemented *)

(** This function evaluates a module structure. *)
and run_structure state ctx =
  let run_item (ctx, _) = run_structure_item state ctx in
  BatList.fold_left run_item (ctx, Value.Int 0)

(** This function computes a value from its allocation in the state. *)
and value_of_alloc state = function
| State.Normal v -> v
| State.Prealloc (exp, ctx) -> run_expression state ctx exp
