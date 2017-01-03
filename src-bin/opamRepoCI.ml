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

  let opam_repo = Repo.v ~user:"ocaml" ~repo:"opam-repository"
  let opam_repo_branch = "master"
  let opam_repo_remote = opam_repo, opam_repo_branch
  let primary_ocaml_version = "4.04.0"
  let compiler_variants = ["4.02.3";"4.03.0";"4.04.0_flambda"]

  let label = "opamRepo"
  let docker_t = DO.v ~logs ~label ~jobs:24 ()
  let opam_v1_t = Opam_build.v ~logs ~label ~version:`V1
  let opam_v2_t = Opam_build.v ~logs ~label ~version:`V2

  let repo_builder_v1 target =
    let packages = Opam_ops.packages_from_diff docker_t target in
    let build = Opam_ops.distro_build ~typ:`Repo ~opam_repo:opam_repo_remote ~opam_t:opam_v1_t ~docker_t in
    let build_revdeps = Opam_ops.V1.run_revdeps in
    let extra_remotes = [] in
    Opam_ops.run_phases ~label:"V1.2" ~extra_remotes ~packages ~build ~build_revdeps docker_t target 

  let repo_builder_v2 target =
    let packages = Opam_ops.packages_from_diff docker_t target in
    let build = Opam_ops.distro_build ~typ:`Repo ~opam_repo:opam_repo_remote ~opam_t:opam_v2_t ~docker_t in
    let build_revdeps = Opam_ops.V2.run_revdeps in
    let extra_remotes = [] in
    Opam_ops.run_phases ~label:"V2.0" ~extra_remotes ~packages ~build ~build_revdeps docker_t target 

  let run_phases target =
    (repo_builder_v1 target) @
    (repo_builder_v2 target)
 
  let tests = [
    Config.project ~id:"ocaml/opam-repository" run_phases
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

