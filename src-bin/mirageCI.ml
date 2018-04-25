(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring

open Datakit_ci
open Datakit_github
module DO = Docker_ops

module Builder = struct

  open Term.Infix

  let label = "mir"
  let docker_t = DO.v ~logs ~label ~jobs:4 ()
  let opam_t = Opam_build.v ~logs ~label
  let mirage_dev = [Opam_docker.repo ~user:"mirage" ~repo:"mirage-dev" ~branch:"master"]

  let packages_of_repo target =
    let repo = Target.repo target in
    match Fmt.strf "%a" Repo.pp repo with
    | "mirage/ocaml-cohttp" -> Term.return ["cohttp"]
    | "mirage/mirage" -> Term.return ["mirage";"mirage-types";"mirage-types-lwt"]
    | "mirage/ocaml-git" -> Term.return ["git";"git-http";"git-unix";"git-mirage"]
    | _ -> Term.fail "Unknown repository for packages_of_repo"

  let repo_builder ~revdeps ~opam_version ~typ ~remotes target =
    let packages = match typ with
      |`Package -> packages_of_repo target
      |`Repo |`Full_repo -> Opam_ops.packages_from_diff docker_t target in
    let opam_repo = Opam_docker.mirage_opam_repository in
    Opam_ops.run_phases ~mode:(`Revdeps revdeps) ~packages ~remotes ~typ ~opam_version ~opam_repo opam_t docker_t target

  let run_phases typ remotes target =
    let all_tests = repo_builder ~opam_version:`V1 ~typ ~remotes target in
    match Target.id target with
    |`Ref ["heads";"master"] -> all_tests ~revdeps:false
    |`PR _  -> all_tests ~revdeps:true
    | _ -> []

  let run_git_phases target =
    let all_tests = repo_builder ~revdeps:true ~opam_version:`V1 ~typ:`Package target in
    match Target.id target with
    |`Ref ["heads";"mirage-dev"] -> all_tests ~remotes:mirage_dev
    |`Ref ["heads";"master"] -> all_tests ~remotes:[]
    |`Ref _ -> []
    |`PR _  -> all_tests ~remotes:mirage_dev


  let tests = [
    Config.project ~id:"mirage/mirage" (run_phases `Package mirage_dev);
    Config.project ~id:"mirage/ocaml-git" run_git_phases;
  ]
end

(* Command-line parsing *)

let web_config =
  Web.config
    ~name:"mirage-ci"
    ~can_read:ACL.(everyone)
    ~can_build:ACL.(github_org "mirage")
    ~state_repo:(Uri.of_string "https://github.com/mirage/mirage-ci.logs")
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
