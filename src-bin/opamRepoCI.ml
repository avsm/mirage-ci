(*---------------------------------------------------------------------------
   Copyright (c) 2017 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring

open Datakit_ci
open Term.Infix
module DO = Docker_ops

module Builder = struct

  let label = "opamRepo"
  let docker_t = DO.v ~logs ~label ~jobs:32 ()
  let opam_t = Opam_build.v ~logs ~label
  let volume = Fpath.v "opam2-archive"

  let repo_builder ~build_filter ~revdeps ~typ target =
    let packages = Opam_ops.packages_from_diff ~default:[] docker_t target in
    let opam_repo = Opam_docker.ocaml_opam_repository in
    Opam_ops.run_phases ~volume ~build_filter ~revdeps ~packages ~remotes:[] ~typ ~opam_repo opam_t docker_t target

  let repo_builder ~revdeps ~typ target =
    let build_filter =
      Term.target target >>= function
      | `PR {Datakit_github.PR.base = "master"; _} -> Term.return true
      | `PR _ | `Ref _ -> Term.return false
    in
    repo_builder ~build_filter ~revdeps ~typ target

  let archive_builder target =
    let t =
      Term.head target >>= fun {Datakit_github.Commit.hash;_} ->
      Opam_ops.Cmds.build_archive ~volume:Fpath.(v "opam2-archive") docker_t hash >>= fun (_img, r) ->
      Term.return r in
    ["archive",t]

  let run_phases typ target =
    match Target.id target with
    |`Ref ["heads";"master"]  -> archive_builder target
    |`Ref _  -> []
    |`PR _ -> repo_builder ~revdeps:true ~typ target

  let tests = [
    Config.project ~id:"ocaml/opam-repository" (run_phases `Full_repo);
  ]
end

(* Command-line parsing *)

let web_config =
  Web.config
    ~name:"opam-repo-ci"
    ~can_read:ACL.(everyone)
    ~can_build:ACL.(everyone)
    ~github_scopes_needed:[]
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
