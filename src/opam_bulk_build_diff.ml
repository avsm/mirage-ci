(*---------------------------------------------------------------------------
   Copyright (c) 2017 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Datakit_ci
open! Astring
open Lwt.Infix

let src = Logs.Src.create "datakit-ci.opam-bulk-diff" ~doc:"Opam_bulk_build diff plugin for Datakit_ci"
module Log = (val Logs.src_log src : Logs.LOG)

let ( / ) = Datakit_path.Infix.( / )

module Opam_bulk_build_diff_key = struct
 open Opam_bulk_build
 type t = 
  | Remote of { ocaml_version: string; distro: string; o: keys; n: keys }
  | OCaml_version of { ocaml_version_a: string; ocaml_version_b: string; distro: string; o: keys; n: keys }
 let compare = Pervasives.compare
end

module Opam_bulk_builder_diff = struct
  type t = {
    label: string;
  }

  module Key = Opam_bulk_build_diff_key

  type context = job_id
  type value = string

  let name t = "opam-bulk-diff:" ^ t.label

  let title _t = 
    function
    | Opam_bulk_build_diff_key.(Remote {o;n;ocaml_version;distro}) ->
        Fmt.strf "Diffing %d -> %d packages (%s-%s)"
         (List.length o) (List.length n) distro ocaml_version
    | Opam_bulk_build_diff_key.(OCaml_version {o;n;ocaml_version_a;ocaml_version_b;distro}) ->
        Fmt.strf "Diffing %d -> %d packages (%s %s->%s)"
         (List.length o) (List.length n) distro ocaml_version_a ocaml_version_b

  let generate t ~switch ~log trans job_id j =
    let buf = Buffer.create 1024 in
    let ppf = Format.formatter_of_buffer buf in
    (match j with
     | Opam_bulk_build_diff_key.(Remote {o;n;ocaml_version;distro}) ->
          Opam_bulk_build.diff ~ocaml_version ~distro o n ppf;
     | Opam_bulk_build_diff_key.(OCaml_version {o;n;ocaml_version_a;ocaml_version_b;distro}) ->
          Opam_bulk_build.diff_by_ocaml_version (ocaml_version_a,ocaml_version_b) ~distro o n ppf);
    let s = Buffer.contents buf in
    Live_log.write log s;
    let open Utils.Infix in
    DK.Transaction.create_or_replace_file trans (Cache.Path.value / "results.txt")
      (Cstruct.of_string s) >>*= fun () ->
    Lwt.return (Ok s)

  let branch _t =
    function
    | Opam_bulk_build_diff_key.(Remote {o;n;ocaml_version;distro}) ->
      Fmt.strf "opam-bulk-diff-%s-%s-%s" ocaml_version distro 
      ((Sexplib.Sexp.to_string_hum (Opam_bulk_build.sexp_of_keys o) ^ (Sexplib.Sexp.to_string_hum (Opam_bulk_build.sexp_of_keys n))) |>
      Digest.string |> Digest.to_hex)
    | Opam_bulk_build_diff_key.(OCaml_version {o;n;ocaml_version_a;ocaml_version_b;distro}) ->
      Fmt.strf "opam-bulk-diff-%s-%s-%s-%s" ocaml_version_a ocaml_version_b distro 
      ((Sexplib.Sexp.to_string_hum (Opam_bulk_build.sexp_of_keys o) ^ (Sexplib.Sexp.to_string_hum (Opam_bulk_build.sexp_of_keys n))) |>
      Digest.string |> Digest.to_hex)
      

  let load _t tr _key =
    let open Utils.Infix in
    DK.Tree.read_file tr (Datakit_path.of_string_exn "value/results.txt") >>*= fun output ->
    Lwt.return (Cstruct.to_string output |> String.trim)
end
 
module Opam_bulk_build_diff_cache = Cache.Make(Opam_bulk_builder_diff)

type t = Opam_bulk_build_diff_cache.t 
let v ~label = Opam_bulk_build_diff_cache.create { Opam_bulk_builder_diff.label }

let run_remote_diff ~ocaml_version ~distro o n config =
  let open! Term.Infix in
  Term.job_id >>= fun job_id ->
  let t = Opam_bulk_build_diff_key.(Remote {ocaml_version; distro; o; n}) in
  Opam_bulk_build_diff_cache.find config job_id t

let run_ocaml_version_diff (ocaml_version_a,ocaml_version_b) ~distro o n config =
  let open! Term.Infix in
  Term.job_id >>= fun job_id ->
  let t = Opam_bulk_build_diff_key.(OCaml_version {ocaml_version_a; ocaml_version_b; distro; o; n}) in
  Opam_bulk_build_diff_cache.find config job_id t

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
