(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring
open Datakit_ci
open Lwt.Infix

let src = Logs.Src.create "datakit-ci.toml" ~doc:"TOML reader plugin for Datakit_ci"
module Log = (val Logs.src_log src : Logs.LOG)

let ( / ) = Datakit_path.Infix.( / )

module Git_key = struct
  type t = Git.commit
  let compare a b = String.compare (Git.hash a) (Git.hash b)
end

module Toml_utils = struct
  let of_cstruct c =
    Cstruct.to_string c |>
    Sexplib.Sexp.of_string |>
    Datakit_toml.l_of_sexp

  let to_cstruct d =
    Datakit_toml.sexp_of_l d |> fun s ->
    Sexplib.Sexp.to_string_hum s |> fun s ->
    Cstruct.of_string s
end

module Toml_builder = struct
  type t = {
    label: string;
    toml_filename: string;
    git_t: Git.t;
  }

  module Key = Git_key
  type context = job_id
  type value = Datakit_toml.t list

  let name t = "toml-build:" ^ t.label

  let title t commit = Git.hash commit

  let generate {label;git_t;toml_filename} ~switch:_ ~log trans job_id commit =
    Git.with_checkout ~job_id ~log commit
     (fun file ->
       let fname = Fmt.strf "%s/%s" file toml_filename in
       Live_log.write log (Fmt.strf "reading file from %s\n" fname);
       let open Rresult.R.Infix in
       match Bos.OS.File.read (Fpath.v fname) >>= Datakit_toml.of_string with
       | Error (`Msg msg) ->
          Live_log.write log (Fmt.strf "Failed to get toml: %s\n" msg);
          Lwt.return []
       | Ok t -> Lwt.return t
     ) >>= fun toml ->
    let open Utils.Infix in
    let output = Live_log.write log in
    Live_log.log log "Storing TOML";
    let data = Toml_utils.to_cstruct toml in
    DK.Transaction.create_or_replace_file trans (Cache.Path.value / "toml.sexp") data >>*= fun () ->
    output (Cstruct.to_string data);
    Lwt.return (Ok toml)

  let branch {label;git_t} commit =
    Fmt.strf "opam-toml-%s" (Git.hash commit)

  let load _t tr _k =
    let open Utils.Infix in
    DK.Tree.read_file tr (Datakit_path.of_string_exn "value/toml.sexp") >>*= fun data ->
    Lwt.return (Toml_utils.of_cstruct data)
end

module Toml_cache = Cache.Make(Toml_builder)
type t = Toml_cache.t

let config ~logs ~label ~toml_filename ~git_t =
  Toml_cache.create ~logs { Toml_builder.label; toml_filename; git_t }

let run config commit =
  let open! Term.Infix in
  Term.job_id >>= fun job_id ->
  Toml_cache.find config job_id commit

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

