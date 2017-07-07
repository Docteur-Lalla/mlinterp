open Batteries

(** Type of an execution context. It contains a map between identifiers and
 * the index of the state where the value is stored. It contains also a list of
 * the modules that have been opened. *)
type t = {
  map : (string, int) BatMap.t ;
  opened_modules : ((string, int) BatMap.t) list ;
}

(** An empty context. *)
let empty = { map = BatMap.empty ; opened_modules = [] }

(** Add the binding (id, idx) to the context and return the new context. *)
let add id idx ctx = { ctx with map = BatMap.add id idx ctx.map }

(** Find the variable id in the context and return the corresponding index.
 * Raise a Not_found exception if no id is found in the context. *)
let find id ctx = BatList.fold_right BatMap.union ctx.opened_modules ctx.map
  |> BatMap.find id

(** Check if the given id is in the map. *)
let member id ctx = BatMap.mem id ctx.map
