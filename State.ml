open Batteries

type alloc =
| Normal of Value.t
| Prealloc of Parsetree.expression * Context.t

type t = alloc BatDynArray.t

let empty () = BatDynArray.create ()

let add state v = BatDynArray.add state v ; BatDynArray.length state - 1
let get = BatDynArray.get
let set = BatDynArray.set
