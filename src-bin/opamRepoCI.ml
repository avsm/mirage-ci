(*---------------------------------------------------------------------------
   Copyright (c) 2017 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring

open Datakit_ci
open Datakit_github
module DO = Docker_ops

module Builder = struct

  open Term.Infix

  let label = "opamRepo"
  let docker_t = DO.v ~logs ~label ~jobs:24 ()
  let opam_t = Opam_build.v ~logs ~label

  let packages_of_repo {Repo.user;repo} =
    match user, repo with
    | "ocaml","opam-repository" -> ["lwt";"async";"coq";"mirage";"datakit"]
    | "janestreet","opam-repository" -> ["async";"async_ssl";"jenga";"jane-street-tests"]
    | "mirage","mirage-dev" -> ["mirage.dev~mirage";"mirage-types";"mirage-types-lwt";"irmin"]
    | _ -> ["ocamlfind"]

  let repo_builder ~revdeps ~typ ~opam_version target =
    let default = packages_of_repo (Target.repo target) in
    let packages = Opam_ops.packages_from_diff ~default docker_t target in
    let opam_repo = Opam_docker.ocaml_opam_repository in
    Opam_ops.run_phases ~revdeps ~packages ~remotes:[] ~typ ~opam_version ~opam_repo opam_t docker_t target

  let run_phases typ target =
    let tests ~revdeps =
      (repo_builder ~revdeps:false ~typ ~opam_version:`V1 target) @
      (repo_builder ~revdeps ~typ ~opam_version:`V2 target)
    in
    let volume1 = Fpath.v "opam-archive" in
    let volume2 = Fpath.v "opam2-archive" in
    let archive_v1 = "Archive v1.2", (
      Term.target target >>= fun target ->
      Commit.hash (Target.head target) |>
      Opam_ops.V1.build_archive ~volume:volume1 docker_t) in
    let archive_v2 = "Archive v2.0", (
      Term.target target >>= fun target ->
      Commit.hash (Target.head target) |>
      Opam_ops.V2.build_archive ~volume:volume2 docker_t) in
    match Target.id target with
    |`Ref ["heads";"master"] ->
       let base_tests = tests ~revdeps:false in
       let archives =
         match Target.repo target with
         | {Repo.repo="opam-repository"; user="ocaml"} -> [archive_v1;archive_v2]
         | _ -> [] in
       archives @ base_tests
    |`Ref _  -> []
    |`PR _ -> tests ~revdeps:true
 
  let tests = [
    Config.project ~id:"ocaml/opam-repository" (run_phases `Full_repo);
    Config.project ~id:"janestreet/opam-repository" (run_phases `Repo);
    Config.project ~id:"mirage/mirage-dev" (run_phases `Repo);
  ]
end

(* Command-line parsing *)

let web_config =
  Web.config
    ~name:"opam-repo-ci"
    ~can_read:ACL.(everyone)
    ~can_build:ACL.(github_org "mirage")
    ~state_repo:(Uri.of_string "https://github.com/ocaml/ocaml-ci.logs")
    ()

let () =
  run (Cmdliner.Term.pure (Config.v ~web_config ~projects:Builder.tests))

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy
   Copyright (c) 2016 Thomas Leonard

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

