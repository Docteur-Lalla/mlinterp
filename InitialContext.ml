open Batteries

let raise_func = Value.Function (fun v -> raise @@ Interpreter.ExceptionRaised v)
let invalid_arg_func = Value.Function (fun s -> raise @@ Interpreter.ExceptionRaised (Value.Sumtype ("Invalid_argument", Some s)))
let failwith_func = Value.Function (fun s -> raise @@ Interpreter.ExceptionRaised (Value.Sumtype ("Failure", Some s)))

let id x = x

let wrap_function unwrap_val wrap_val op = Value.Function (wrap_val % op % unwrap_val)
let wrap_function2 unwrap1 unwrap2 wrap op =
  let inner v1 = wrap_function unwrap2 wrap (op v1) in
  let outer wv1 = inner (unwrap1 wv1) in
  Value.from_function outer

let int_int_function = wrap_function Value.to_int Value.from_int
let float_float_function = wrap_function Value.to_float Value.from_float

let int_binary_operator = wrap_function2 Value.to_int Value.to_int Value.from_int
let float_binary_operator = wrap_function2 Value.to_float Value.to_float Value.from_float

let bool_binary_operator op =
  let bool_of_string = function
  | "true" -> true
  | "false" -> false
  | _ -> false in

  let string_of_bool = function
  | true -> "true"
  | false -> "false" in

  let outer = function
  | Value.Sumtype ("true" as b1s, None)
  | Value.Sumtype ("false" as b1s, None) ->
    let inner = function
    | Value.Sumtype ("true" as b2s, None)
    | Value.Sumtype ("false" as b2s, None) ->
      let b1 = bool_of_string b1s in
      let b2 = bool_of_string b2s in
      Value.Sumtype (string_of_bool @@ op b1 b2, None)
    | _ -> raise Value.TypeError in
    Value.Function inner
  | _ -> raise Value.TypeError in
  Value.Function outer

let equal =
  let outer v1 =
    let inner v2 = Value.Sumtype (string_of_bool @@ ValueUtils.value_eq v1 v2, None) in
    Value.Function inner in
  Value.Function outer

let not_equal =
  let outer v1 =
    let inner v2 = Value.Sumtype (string_of_bool @@ not @@ ValueUtils.value_eq v1 v2, None) in
    Value.Function inner in
  Value.Function outer

let initial_context = [
  ("raise", raise_func) ;
  ("raise_notrace", raise_func) ;
  ("invalid_arg", invalid_arg_func) ;
  ("failwith", failwith_func) ;

  ("=", equal) ;
  ("<>", not_equal) ;

  ("+", int_binary_operator ( + )) ;
  ("-", int_binary_operator ( - )) ;
  ("*", int_binary_operator ( * )) ;
  ("/", int_binary_operator ( / )) ;
  ("mod", int_binary_operator ( mod ));

  ("abs", int_int_function abs) ;
  ("succ", int_int_function succ) ;
  ("pred", int_int_function pred) ;
  ("~-", int_int_function (~-)) ;
  ("~+", int_int_function (~-)) ;

  ("land", int_binary_operator (land)) ;
  ("lor", int_binary_operator (lor)) ;
  ("lxor", int_binary_operator (lxor)) ;
  ("lsl", int_binary_operator (lsl)) ;
  ("lsr", int_binary_operator (lsr)) ;
  ("asr", int_binary_operator (asr)) ;
  ("lnot", int_int_function lnot) ;

  ("+.", float_binary_operator ( +. )) ;
  ("-.", float_binary_operator ( -. )) ;
  ("*.", float_binary_operator ( *. )) ;
  ("/.", float_binary_operator ( /. )) ;
  ("**", float_binary_operator ( ** )) ;

  ("sqrt", float_float_function ( sqrt )) ;
  ("exp", float_float_function ( exp )) ;
  ("log", float_float_function ( log )) ;
  ("log10", float_float_function ( log10 )) ;
  ("expm1", float_float_function ( expm1 )) ;
  ("log1p", float_float_function ( log1p )) ;
  ("cos", float_float_function ( cos )) ;
  ("sin", float_float_function ( sin )) ;
  ("tan", float_float_function ( tan )) ;
  ("acos", float_float_function ( acos )) ;
  ("asin", float_float_function ( asin )) ;
  ("atan", float_float_function ( atan )) ;
  ("atan2", float_binary_operator ( atan2 )) ;
  ("cosh", float_float_function ( cosh )) ;
  ("sinh", float_float_function ( sinh )) ;
  ("tanh", float_float_function ( tanh )) ;
  ("ceil", float_float_function ( ceil )) ;
  ("floor", float_float_function ( floor )) ;
  ("abs_float", float_float_function ( abs_float )) ;

  ("&&", bool_binary_operator ( && )) ;
  ("||", bool_binary_operator ( || ))
]

let populate state =
  let func ctx (name, value) =
    let idx = State.add state (State.Normal value) in
    Context.add name idx ctx in
  let map = Context.to_map @@ BatList.fold_left func Context.empty initial_context in
  let idx = State.add state (State.Normal (Value.Module map)) in
  let ctx = Context.add "Pervasives" idx Context.empty in
  Context.open_module map ctx
