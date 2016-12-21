(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring

open Datakit_ci
open Datakit_github

module Builder = struct

  open Term.Infix

  let one_hour = 60. *. 60.
  let eight_hours = one_hour *. 8.
  let opam_repo = Repo.v ~user:"ocaml" ~repo:"opam-repository"

  let pool = Monitored_pool.create "docker" 24
  let label = "ocaml" 
  let docker_t = Docker_build.v ~network:"mirageci_opam_build" ~logs ~label ~pool ~timeout:eight_hours ()
  let docker_run_t = Docker_run.config ~logs ~label ~pool ~timeout:eight_hours
  let opam_t = Opam_build.v ~logs ~label ~version:`V2

  let opam_build_all target =
    let distro = "ubuntu-16.04" in
    let base_dfile ~ocaml_version ~git_rev = 
      let open Dockerfile in
      from ~tag:(distro^"_ocaml-"^ocaml_version) "ocaml/opam" @@
      workdir "/home/opam/opam-repository" @@
      run "git pull origin master" @@
      run "git checkout %s" git_rev @@
      run "opam repo set-url default http://opamarchive:8081" @@
      run "opam update" @@
      run "opam depext -uivy ocamlfind ocamlbuild camlp4"
    in
    let bulk_build ~ocaml_version =
      Term.head target >>= fun h -> 
      let git_rev = Commit.hash h in
      Docker_build.run docker_t ~hum:(Fmt.strf "Base for %s (%s)" ocaml_version git_rev) (base_dfile ~ocaml_version ~git_rev)
      >>= fun img -> Opam_ops.list_all_packages docker_run_t img
      >>= Opam_ops.build_packages docker_t img
    in
    let archive_build =
      Term.head target >>= fun h ->
      let git_rev = Commit.hash h in
      Opam_ops.V1.build_archive ~volume:(Fpath.v "opam-archive") docker_t docker_run_t git_rev
    in
    let archive_build_v2 =
      Term.head target >>= fun h ->
      let git_rev = Commit.hash h in
      Opam_ops.V2.build_archive ~volume:(Fpath.v "opam2-archive") docker_t docker_run_t git_rev
    in
    let all_tests = [
(*
      Term_utils.report ~order:1 ~label:"4.03.0" (bulk_build ~ocaml_version:"4.03.0");
      Term_utils.report ~order:2 ~label:"4.04.0" (bulk_build ~ocaml_version:"4.04.0");
      Term_utils.report ~order:3 ~label:"4.02.3" (bulk_build ~ocaml_version:"4.02.3");
*)
      Term_utils.report ~order:1 ~label:"opam1 archive" archive_build;
      Term_utils.report ~order:2 ~label:"opam2 archive" archive_build_v2;
    ] in
    match Target.id target with
    |`Ref ["heads";"bulk"] -> all_tests
    | _ -> []

  let tests = [
    Config.project ~id:"mirage/opam-repository" opam_build_all;
  ]
end

(* Command-line parsing *)

let web_config =
  Web.config
    ~name:"opam-ci"
    ~can_read:ACL.(everyone)
    ~can_build:ACL.(username "admin")
    ~state_repo:(Uri.of_string "https://github.com/mirage/opam-ci.logs")
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

