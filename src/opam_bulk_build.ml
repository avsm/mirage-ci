(*---------------------------------------------------------------------------
   Copyright (c) 2016-2017 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Datakit_ci
open! Astring
open Lwt.Infix

let src = Logs.Src.create "datakit-ci.opam-bulk" ~doc:"Opam_bulk_build plugin for Datakit_ci"
module Log = (val Logs.src_log src : Logs.LOG)

let ( / ) = Datakit_path.Infix.( / )

open Sexplib.Conv
type key = {
  ocaml_version: string;
  distro: string;
  package: string;
  success: bool;
  log_branch: string;
} [@@deriving sexp]

type keys = key list [@@deriving sexp]

let compare = Pervasives.compare

module Opam_bulk_build_key = struct
 type t = keys
 let compare = Pervasives.compare
end

module Opam_bulk_builder = struct
  type t = {
    label: string;
  }

  module Key = Opam_bulk_build_key

  type context = job_id
  type value = string

  let name t = "opam-bulk:" ^ t.label

  let title _t keys = Fmt.strf "Reporting %d results" (List.length keys)

  let generate t ~switch ~log trans job_id results =
    let s = Sexplib.Sexp.to_string_hum (sexp_of_keys results) in
    Live_log.write log s;
    let open Utils.Infix in
    DK.Transaction.create_or_replace_file trans (Cache.Path.value / "results.sexp")
      (Cstruct.of_string s) >>*= fun () ->
    Lwt.return (Ok (Fmt.strf "%d packages built" (List.length results)))

  let branch _t results =
    Sexplib.Sexp.to_string_hum (sexp_of_keys results) |>
    Digest.string |> Digest.to_hex |> Fmt.strf "opam-bulk-%s"

  let load _t tr _key =
    let open Utils.Infix in
    DK.Tree.read_file tr (Datakit_path.of_string_exn "value/results.sexp") >>*= fun output ->
    Lwt.return (Cstruct.to_string output)
end
 
module Opam_bulk_build_cache = Cache.Make(Opam_bulk_builder)

type t = Opam_bulk_build_cache.t 
let v ~label = Opam_bulk_build_cache.create { Opam_bulk_builder.label }

let run config results =
  let open! Term.Infix in
  Term.job_id >>= fun job_id ->
  Opam_bulk_build_cache.find config job_id results

module K = struct
  type t = key
    let ( ++ ) x fn =
    match x with
    | 0 -> fn ()
    | r -> r

  let split_name p =
    match String.cut ~sep:"." p with
    |Some (k,v) -> k,v
    |None -> p,"0"

  let compare a {ocaml_version;distro;package;success;log_branch} =
    compare ocaml_version a.ocaml_version ++ fun () ->
    compare distro a.distro ++ fun () ->
    compare package a.package ++ fun () ->
    compare success a.success
end

module P = Map.Make(String)
 
let diff ~ocaml_version ~distro a b ppf =
  let a = List.filter (fun t -> t.ocaml_version = ocaml_version && t.distro = distro) a in
  let b = List.filter (fun t -> t.ocaml_version = ocaml_version && t.distro = distro) b in
  let pold =
    List.fold_left (fun m p ->
      let name, version = K.split_name p.package in
      if P.mem name m then failwith (Fmt.strf "duplicate package %s" p.package);
      P.add name p m) P.empty a in
  let pnew = 
    List.fold_left (fun m p ->
      let name, version = K.split_name p.package in
      if P.mem name m then failwith (Fmt.strf "duplicate package %s" p.package);
      P.add name p m) P.empty b in
  P.iter (fun name p ->
    match P.find name pnew with
    |p' when K.compare p p' <> 0 -> begin
      let _, v1 = K.split_name p.package in
      let _, v2 = K.split_name p'.package in
      match p.success, p'.success with
      |true, false -> Fmt.pf ppf "%s (%s -> %s) fails now\n" name v1 v2
      |false, true -> Fmt.pf ppf "%s (%s -> %s) broken before and now builds\n" name v1 v2
      |false, false -> Fmt.pf ppf "%s (%s -> %s) still broken\n" name v1 v2
      |true, true -> ()
    end
    |p' -> ()
    |exception Not_found ->
      let _, v1 = K.split_name p.package in
      Fmt.pf ppf "%s (%s) is now uninstallable\n" name v1
  ) pold;
  P.iter (fun name p ->
   match P.find name pold with
   |_ -> ()
   |exception Not_found -> begin
     let _, v1 = K.split_name p.package in
     match p.success with
     |false -> Fmt.pf ppf "%s (new %s) fails now\n" name v1
     |true -> ()
   end
  ) pnew
 
(*---------------------------------------------------------------------------
   Copyright (c) 2016-2017 Anil Madhavapeddy

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
