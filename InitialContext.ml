open Batteries

let raise_func = Value.Function (fun v -> raise @@ Interpreter.ExceptionRaised v)
let invalid_arg_func = Value.Function (fun s -> raise @@ Interpreter.ExceptionRaised (Value.Sumtype ("Invalid_argument", Some s)))
let failwith_func = Value.Function (fun s -> raise @@ Interpreter.ExceptionRaised (Value.Sumtype ("Failure", Some s)))

let int_binary_operator op =
  let outer = function
  | Value.Int i1 ->
    let inner = function
    | Value.Int i2 -> Value.Int (op i1 i2)
    | _ -> raise Value.TypeError in
    Value.Function inner
  | _ -> raise Value.TypeError in
  Value.Function outer

let float_binary_operator op =
  let outer = function
  | Value.Float f1 ->
    let inner = function
    | Value.Float f2 -> Value.Float (op f1 f2)
    | _ -> raise Value.TypeError in
    Value.Function inner
  | _ -> raise Value.TypeError in
  Value.Function outer

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

let initial_context = [
  ("raise", raise_func) ;
  ("raise_notrace", raise_func) ;
  ("invalid_arg", invalid_arg_func) ;
  ("failwith", failwith_func) ;

  ("+", int_binary_operator ( + )) ;
  ("-", int_binary_operator ( - )) ;
  ("*", int_binary_operator ( * )) ;
  ("/", int_binary_operator ( / )) ;
  ("mod", int_binary_operator ( mod ));

  ("+.", float_binary_operator ( +. )) ;
  ("-.", float_binary_operator ( -. )) ;
  ("*.", float_binary_operator ( *. )) ;
  ("/.", float_binary_operator ( /. )) ;

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
