(*
 * Copyright 2019 Kévin Le Bon <kevin.le-bon@inria.fr>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *)

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
let member id ctx = BatList.map (BatMap.mem id) ctx.opened_modules
  |> BatList.fold_left (||) (BatMap.mem id ctx.map)

(** Retrieve the inner map of a context. *)
let to_map ctx = ctx.map

(** Build a context from the given map. *)
let from_map map = { map = map ; opened_modules = [] }

(** Include every binding of ctx' in ctx. *)
let include_context ctx' ctx =
  let map = BatMap.union ctx'.map ctx.map in
  let op = ctx'.opened_modules @ ctx.opened_modules in
  { map = map ; opened_modules = op }

(** Add the given module to the list of the opened ones *)
let open_module md ctx = { ctx with opened_modules = md :: ctx.opened_modules }
