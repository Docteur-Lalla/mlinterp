open Batteries

type t = {
  map : (string, int) BatMap.t ;
  opened_modules : ((string, int) BatMap.t) list ;
}

let empty = { map = BatMap.empty ; opened_modules = [] }
let add id idx ctx = { ctx with map = BatMap.add id idx ctx.map }

let find id ctx = BatList.fold_right BatMap.union ctx.opened_modules ctx.map
  |> BatMap.find id

let member id ctx = BatMap.mem id ctx.map
