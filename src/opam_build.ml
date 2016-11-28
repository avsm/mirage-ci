(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring
open DataKitCI

let src = Logs.Src.create "datakit-ci.opam" ~doc:"OPAM plugin for DataKitCI"
module Log = (val Logs.src_log src : Logs.LOG)

let ( / ) = Datakit_path.Infix.( / )

module Dockerfile_utils = struct
  let of_cstruct c =
    Cstruct.to_string c |>
    Sexplib.Sexp.of_string |>
    Dockerfile.t_of_sexp

  let to_cstruct d =
    Dockerfile.sexp_of_t d |> fun s ->
    Sexplib.Sexp.to_string_hum s |> fun s ->
    Cstruct.of_string s
end

type key = {
  package: string;
  target: [`PR of Github_hooks.PR.t | `Ref of Github_hooks.Ref.t ];
  distro: string;
  ocaml_version: string;
  remote_git_rev: string;
  extra_remotes: (ProjectID.t * string * Github_hooks.Commit.t) list;
}

module Opam_key = struct

  type t = key

  let compare_target a b =
    match a,b with
    |`PR a, `PR b -> Github_hooks.PR.compare a b
    |`Ref a, `Ref b -> Github_hooks.Ref.compare a b
    |a,b -> Pervasives.compare a b

  let compare a b =
    match compare_target a.target b.target with
    | 0 -> Pervasives.compare (a.distro,a.ocaml_version) (b.distro,b.ocaml_version)
    | x -> x
end


module Opam_builder = struct
  type t = {
    label : string;
  }

  module Key = Opam_key
  type context = NoContext
  type value = Dockerfile.t

  let id_of_target = function
    | `PR pr -> string_of_int (Github_hooks.PR.id pr)
    | `Ref rf -> Datakit_path.to_hum (Github_hooks.Ref.name rf)

  let project_of_target = function
    | `PR pr -> Github_hooks.PR.project pr
    | `Ref rf -> Github_hooks.Ref.project rf

  let head_of_target = function
    | `PR pr -> Github_hooks.PR.head pr
    | `Ref rf -> Github_hooks.Ref.head rf

  let branch_of_target = function
    | `PR pr -> Printf.sprintf "pull/%d/head" (Github_hooks.PR.id pr)
    | `Ref r -> Github_hooks.Ref.name r |> Datakit_path.to_hum

  let name t = "opam:" ^ t.label

  let title t {target;package;distro;ocaml_version;remote_git_rev;extra_remotes} =
    let id = id_of_target target in
    let remotes =
      String.concat ~sep:":" (
       List.map (fun (pid,_,com) ->
         Fmt.strf "%a:%s" ProjectID.pp pid
          (String.with_range ~len:6 (Github_hooks.Commit.hash com))
       ) extra_remotes)
      in
    Fmt.strf "Dockerfile for %s %s:%s (%s/%s/opam-repo:%s:%s)"
      package t.label id distro ocaml_version
      (String.with_range ~len:6 remote_git_rev) remotes

  let generate _t ~switch:_ ~log trans NoContext {target;package;distro;ocaml_version;remote_git_rev;extra_remotes} =
    let pid = project_of_target target in
    let commit = Github_hooks.Commit.hash (head_of_target target) in
    let remotes_ref = ref 0 in
    let dockerfile =
      let open! Dockerfile in
    let remotes = List.map (fun (pid,_,commit) ->
      incr remotes_ref;
      run "opam remote add extra%d https://github.com/%s.git#%s"
        !remotes_ref (Fmt.strf "%a" ProjectID.pp pid) (Github_hooks.Commit.hash commit)
      ) extra_remotes in
      from ~tag:(distro^"_ocaml-"^ocaml_version) "ocaml/opam" @@
      workdir "/home/opam/opam-repository" @@
      run "git pull origin master" @@
      (run "git checkout %s" remote_git_rev @@@ remotes) @@
      run "opam update" @@
      run "git clone git://github.com/%s/%s /home/opam/src"
        pid.ProjectID.user pid.ProjectID.project @@
      workdir "/home/opam/src" @@
      run "git fetch origin %s:cibranch" (branch_of_target target) @@
      run "git checkout %s" commit @@
      run "opam pin add -n %s /home/opam/src" package @@
      run "opam depext -uy %s" package @@
      run "opam install -j 2 -vy %s" package
    in
    let open Utils in
    let output = Live_log.write log in
    Live_log.log log "Building Dockerfile for installing %s (%s %s)" package distro ocaml_version;
    let data = Dockerfile_utils.to_cstruct dockerfile in
    DK.Transaction.create_or_replace_file trans (Cache.Path.value / "Dockerfile.sexp") data >>*= fun () ->
    output (Dockerfile.string_of_t dockerfile);
    Lwt.return  (Ok dockerfile)

  let branch t {target;package;distro;ocaml_version;remote_git_rev;extra_remotes}=
    let id = id_of_target target in
    let proj = project_of_target target in
    let commit = Github_hooks.Commit.hash (head_of_target target) in
    let extra = String.concat ~sep:":" 
       (List.map (fun (pid,_,commit) -> Fmt.strf "%a#%s"
        ProjectID.pp pid (Github_hooks.Commit.hash commit)) extra_remotes) in
    Fmt.strf "opam-%s-%s-%s-%s-%s-%s-%s-%s-%s-%s"
      proj.ProjectID.user proj.ProjectID.project
      t.label distro ocaml_version id commit remote_git_rev extra package |>
    Digest.string |> Digest.to_hex |> Fmt.strf "opam-build-%s"

  let load _t tr _k =
    let open Utils in
    DK.Tree.read_file tr (Datakit_path.of_string_exn "value/Dockerfile.sexp") >>*= fun data ->
    Lwt.return (Dockerfile_utils.of_cstruct data)

end

module Opam_cache = Cache.Make(Opam_builder)

type t = Opam_cache.t

let config ~logs ~label =
  Opam_cache.create ~logs { Opam_builder.label }

let run config pr =
  Opam_cache.term config Opam_builder.NoContext pr

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
