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
    | "ocaml","opam-repository" -> ["lwt";"async";"coq";"mirage"]
    | "janestreet","opam-repository" -> ["async";"async_ssl";"jenga"]
    | "mirage","mirage-dev" -> ["mirage";"mirage-types";"mirage-types-lwt";"irmin"]
    | _ -> ["ocamlfind"]

  let repo_builder ~remotes ~opam_version target =
    let default = packages_of_repo (Target.repo target) in
    let packages = Opam_ops.packages_from_diff ~default docker_t target in
    let opam_repo = Opam_docker.ocaml_opam_repository in
    let typ = `Repo in
    Opam_ops.run_phases ~packages ~remotes ~typ ~opam_version ~opam_repo opam_t docker_t target

  let run_phases remotes target =
    let tests =
      (repo_builder ~remotes ~opam_version:`V1 target) @
      (repo_builder ~remotes ~opam_version:`V2 target) in
    match Target.id target with
    |`PR _ | `Ref ["heads";"master"] -> tests
    |`Ref _  -> []
 
  let tests = [
    Config.project ~id:"ocaml/opam-repository" (run_phases []);
    Config.project ~id:"janestreet/opam-repository" (run_phases []);
    Config.project ~id:"mirage/mirage-dev" (run_phases []);
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

