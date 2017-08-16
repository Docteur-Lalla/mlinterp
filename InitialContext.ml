open Batteries

let raise_func = Value.Function (fun v -> raise @@ Interpreter.ExceptionRaised v)

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

let initial_context = [
  ("raise", raise_func) ;
  ("+", int_binary_operator ( + )) ;
  ("-", int_binary_operator ( - )) ;
  ("*", int_binary_operator ( * )) ;
  ("/", int_binary_operator ( / )) ;
  ("mod", int_binary_operator ( mod ));

  ("+.", float_binary_operator ( +. )) ;
  ("-.", float_binary_operator ( -. )) ;
  ("*.", float_binary_operator ( *. )) ;
  ("/.", float_binary_operator ( /. ))
]

let populate state =
  let func ctx (name, value) =
    let idx = State.add state (State.Normal value) in
    Context.add name idx ctx in
  BatList.fold_left func Context.empty initial_context
