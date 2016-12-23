(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
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
    let results = Sexplib.Sexp.to_string_hum (sexp_of_keys results) in
    Live_log.write log results;
    let open Utils.Infix in
    DK.Transaction.create_or_replace_file trans (Cache.Path.value / "results.sexp")
      (Cstruct.of_string results) >>*= fun () ->
    Lwt.return (Ok results)

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

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy

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
