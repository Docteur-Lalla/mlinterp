open Batteries

(** Data stored in the state. A normally allocated value is stored computed.
 * A pre-allocated value is stored as an expression with its associated context. *)
type alloc =
| Normal of Value.t
| Prealloc of Parsetree.expression * Context.t

(** Datatype of a program state. *)
type t = alloc BatDynArray.t

(** Create an empty state. *)
let empty () = BatDynArray.create ()

(** Add a value to the state and return its index. *)
let add state v = BatDynArray.add state v ; BatDynArray.length state - 1

(** Get the value from the state at the given index. *)
let get = BatDynArray.get
(** Set the value at the given index in the state. *)
let set = BatDynArray.set
