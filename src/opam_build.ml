(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring
open Datakit_ci
open Datakit_github

let src = Logs.Src.create "datakit-ci.opam" ~doc:"OPAM1 plugin for Datakit_ci"
module Log = (val Logs.src_log src : Logs.LOG)

type key = {
  packages: string list;
  distro: string;
  ocaml_version: string;
  remote_git_rev: string;
  extra_remotes: (Repo.t * Commit.t) list;
  target: Target.v option;
  typ: [`Package | `Repo];
}

module Opam_key = struct

  type t = key

  let ( ++ ) x fn =
    match x with
    | 0 -> fn ()
    | r -> r

  let compare_target a b =
    match a,b with
    |Some a, Some b -> Target.compare_v a b
    |_ -> Pervasives.compare a b

  let compare {packages;target;distro;ocaml_version;remote_git_rev;extra_remotes;typ} b =
    Pervasives.compare packages b.packages ++ fun () ->
    compare_target target b.target ++ fun () ->
    String.compare distro b.distro ++ fun () ->
    String.compare ocaml_version b.ocaml_version ++ fun () ->
    String.compare remote_git_rev b.remote_git_rev ++ fun () ->
    Pervasives.compare typ b.typ ++ fun () ->
    Pervasives.compare extra_remotes b.extra_remotes
end


module Opam_builder = struct
  type t = {
    label : string;
    version : [ `V1 | `V2 ]
  }

  module Key = Opam_key
  type context = NoContext
  type value = Dockerfile.t

  let id_of_target = function
    | `PR pr -> string_of_int (PR.number pr)
    | `Ref rf -> Fmt.strf "%a" Ref.pp_name (Ref.name rf)

  let project_of_target = function
    | `PR pr -> PR.repo pr
    | `Ref rf -> Ref.repo rf

  let head_of_target = Target.head

  let branch_of_target = function
    | `PR pr -> Printf.sprintf "pull/%d/head" (PR.number pr)
    | `Ref r -> Fmt.strf "%a" Ref.pp_name (Ref.name r)

  let name t = "opam:" ^ t.label

  let title t {target;packages;distro;ocaml_version;remote_git_rev;extra_remotes} =
    let remotes =
      String.concat ~sep:":" (
       List.map (fun (pid,com) ->
         Fmt.strf "%a:%s" Repo.pp pid
          (String.with_range ~len:8 (Commit.hash com))
       ) extra_remotes)
      in
    Fmt.strf "Dockerfile %a %s (%s/%s/opam-repo:%s:%s)"
      Fmt.(list string) packages t.label distro ocaml_version
      (String.with_range ~len:6 remote_git_rev) remotes

  let generate t ~switch:_ ~log trans NoContext {target;packages;distro;ocaml_version;remote_git_rev;extra_remotes;typ} =
    let (module OD : Opam_docker.V) =
      match t.version with
      | `V1 -> (module Opam_docker.V1)
      | `V2 -> (module Opam_docker.V2) in
    let target_d =
      match typ,target with
      | _,None -> Dockerfile.empty
      | `Package, Some target ->
         let commit = Commit.hash (head_of_target target) in
         let branch = branch_of_target target in
         let {Repo.user; repo} = project_of_target target in
         let (@@) = Dockerfile.(@@) in (* todo add Dockerfile.Infix *)
         OD.set_opam_repo_rev remote_git_rev @@
         OD.clone_src ~user ~repo ~branch ~commit ~packages
      | `Repo, Some target ->
         let commit = Commit.hash (head_of_target target) in
         let branch = branch_of_target target in
         OD.set_opam_repo_rev ~branch commit
    in
    let dockerfile =
      let open! Dockerfile in
      OD.base ~ocaml_version ~distro @@
      OD.add_remotes extra_remotes @@
      target_d @@
      run "opam update -uy"
    in
    let open Utils.Infix in
    let open Datakit_path.Infix in
    let output = Live_log.write log in
    Live_log.log log "Building Dockerfile for installing %a (%s %s)" (Fmt.(list string)) packages distro ocaml_version;
    let data = Dockerfile_conv.to_cstruct dockerfile in
    DK.Transaction.create_or_replace_file trans (Cache.Path.value / "Dockerfile.sexp") data >>*= fun () ->
    output (Dockerfile.string_of_t dockerfile);
    Lwt.return  (Ok dockerfile)

  let branch t {target;packages;distro;ocaml_version;remote_git_rev;extra_remotes}=
    let targ =
      match target with
      | None -> ""
      | Some target ->
          let id = id_of_target target in
          let proj = project_of_target target in
          let commit = Commit.hash (head_of_target target) in
          Fmt.strf "%s-%a-%s"  id Repo.pp proj commit in
    let extra = String.concat ~sep:":" 
       (List.map (fun (pid,commit) -> Fmt.strf "%a#%s"
        Repo.pp pid (Commit.hash commit)) extra_remotes) in
    Fmt.strf "opam-%s-%s-%s-%s-%s-%s-%a" targ
      t.label distro ocaml_version remote_git_rev extra (Fmt.(list string)) packages |>
    Digest.string |> Digest.to_hex |> Fmt.strf "opam-build-%s"

  let load _t tr _k =
    let open Utils.Infix in
    DK.Tree.read_file tr (Datakit_path.of_string_exn "value/Dockerfile.sexp") >>*= fun data ->
    Lwt.return (Dockerfile_conv.of_cstruct data)

end

module Opam_cache = Cache.Make(Opam_builder)

type t = Opam_cache.t

let v ~version ~logs ~label =
  Opam_cache.create ~logs { Opam_builder.label; version }

let run config pr =
  Opam_cache.find config Opam_builder.NoContext pr

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
