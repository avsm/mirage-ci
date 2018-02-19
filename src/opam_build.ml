(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring
open Datakit_ci
open Datakit_github
open Opam_docker

let src = Logs.Src.create "datakit-ci.opam" ~doc:"OPAM plugin for Datakit_ci"
module Log = (val Logs.src_log src : Logs.LOG)

type key = {
  packages: string list; (* TODO merge this with Packages as only used there *)
  distro: string;
  ocaml_version: string;
  remotes: Remote.t list;
  target: Target.v option;
  typ: [`Package | `Repo | `Full_repo ];
  opam_version: [`V1 | `V2];
}

module Opam_key = struct
  type t = key
end

module Opam_builder = struct
  type t = {
    label : string;
  }

  module Key = Opam_key
  type context = NoContext
  type value = Dockerfile.t

  (* TODO upstream to Datakit_ci *)
  let repo_of_target = function
    | `PR pr -> PR.repo pr
    | `Ref rf -> Ref.repo rf

  let branch_of_target = function
    | `PR pr -> Printf.sprintf "pull/%d/head" (PR.number pr)
    | `Ref r -> Fmt.strf "%a" Ref.pp_name (Ref.name r)

  let name t = "opam:" ^ t.label

  let title t {target;packages;distro;ocaml_version;remotes;opam_version} =
    let sremotes =
      String.concat ~sep:":" (
       List.map (fun {Remote.repo;commit;full_remote} ->
         Fmt.strf "%a:%s" Repo.pp repo
          (String.with_range ~len:8 (Commit.hash commit))
       ) remotes)
      in
    Fmt.strf "Dockerfile %a %s/ocaml-%s/%s)" Fmt.(list ~sep:sp string) packages distro ocaml_version sremotes

  let generate t ~switch:_ ~log trans NoContext {target;packages;distro;ocaml_version;remotes;typ;opam_version} =
    let (module OD:Opam_docker.V) =
      match opam_version with
      | `V1 -> (module Opam_docker.V1)
      | `V2 -> (module Opam_docker.V2) in
    (* The remotes has a full list of all of the remotes, so we need to filter it to selectively
       select the mainline remote (for /home/opam/opam-repository) vs the extra ones *)
    let opam_repo_remotes, remotes = List.partition (fun {Remote.full_remote;_} -> full_remote) remotes in
    let opam_repo_remote =
      match opam_repo_remotes with
      | [hd] -> hd
      | [] -> failwith "No full remote repo found"
      | _ -> failwith "Only one full remote repo is allowed"
    in
    let opam_repo_rev = Commit.hash (opam_repo_remote.Remote.commit) in
    let target_d =
      let (@@) = Dockerfile.(@@) in (* todo add Dockerfile.Infix *)
      match target with
      | None -> (* No target to build so do nothing *)
          Dockerfile.empty
      | Some target ->
          let commit = Commit.hash (Target.head target) in
          let branch = branch_of_target target in
          let {Repo.user; repo} = repo_of_target target in
          let user = Datakit_github.User.name user in
          match typ with
          | `Package -> (* Build and pin an OPAM package repository *)
              OD.set_opam_repo_rev ~remote:opam_repo_remote opam_repo_rev @@
              OD.clone_src ~user ~repo ~branch ~commit @@
              OD.add_local_pins packages
          | `Full_repo ->
              OD.set_opam_repo_rev ~remote:opam_repo_remote opam_repo_rev @@
              OD.merge_src ~user ~repo ~branch ~commit
          | `Repo ->
              OD.set_opam_repo_rev ~remote:opam_repo_remote opam_repo_rev @@
              OD.clone_src ~user ~repo ~branch ~commit @@
              OD.add_local_remote
    in
    let dockerfile =
      let open! Dockerfile in
      OD.base ~ocaml_version ~distro @@
      OD.add_ci_script @@
      OD.add_remotes remotes @@
      target_d @@
      run "opam update -uy"
    in
    let open Utils.Infix in
    let open Datakit_client.Path.Infix in
    let output = Live_log.write log in
    Live_log.log log "Building Dockerfile for installing %a (%s %s)" (Fmt.(list ~sep:sp string)) packages distro ocaml_version;
    let data = Dockerfile_conv.to_cstruct dockerfile in
    DK.Transaction.create_or_replace_file trans (Cache.Path.value / "Dockerfile.sexp") data >>*= fun () ->
    output (Dockerfile.string_of_t dockerfile ^ "\n");
    Lwt.return  (Ok dockerfile)

  let branch t {target;packages;distro;ocaml_version;remotes;opam_version;typ} =
    (* TODO upstream *)
    let target_v_pp ppf t =
      match t with
      |`PR pr -> PR.pp ppf pr
      |`Ref rl -> Ref.pp ppf rl
    in
    let target = Fmt.(strf "%a" (option target_v_pp) target) in
    let remotes = Fmt.(strf "%a" (list Remote.pp_for_compare) remotes) in
    let packages = String.concat ~sep:" " packages in
    let opam_version = match opam_version with `V1 -> "v1" | `V2 -> "v2" in
    let typ = match typ with `Package -> "package" |`Repo -> "repo" |`Full_repo -> "fullrepo" in
    Fmt.strf "%s%s%s%s%s%s%s" target packages distro ocaml_version remotes opam_version typ |>
    Digest.string |> Digest.to_hex |> Fmt.strf "opam-build-%s"

  let load _t tr _k =
    let open Utils.Infix in
    DK.Tree.read_file tr (Datakit_client.Path.of_string_exn "value/Dockerfile.sexp") >>*= fun data ->
    Lwt.return (Dockerfile_conv.of_cstruct data)

end

module Opam_cache = Cache.Make(Opam_builder)

type t = Opam_cache.t

let v ~logs ~label =
  Opam_cache.create ~logs { Opam_builder.label }

let run ?(packages=[]) ?target ~distro ~ocaml_version ~remotes ~typ ~opam_version t =
  let key = { packages; distro; ocaml_version; remotes; target; typ; opam_version } in
  Opam_cache.find t Opam_builder.NoContext key

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
