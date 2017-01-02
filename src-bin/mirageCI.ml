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

  let opam_repo = Repo.v ~user:"mirage" ~repo:"opam-repository"
  let mirage_dev_repo = Repo.v ~user:"mirage" ~repo:"mirage-dev"
  let mirage_dev_branch = "master"
  let mirage_dev_remote = mirage_dev_repo, mirage_dev_branch
  let primary_ocaml_version = "4.04.0"
  let compiler_variants = ["4.03.0";"4.04.0_flambda"]

  let label = "mir"
  let docker_t = DO.v ~logs ~label ~jobs:4 ()
  let opam_t = Opam_build.v ~logs ~label ~version:`V1

  (* XXX TODO temporary until we can query package list automatically *)
  let packages_of_repo target =
    let repo = Target.repo target in
    match Fmt.strf "%a" Repo.pp repo with
    | "mirage/ocaml-cohttp" -> ["cohttp"]
    | "mirage/mirage" -> ["mirage";"mirage-types"]
    | _ -> failwith "TODO package_of_repo"

  (* base building *)
  let build ?(extra_remotes=[]) ?(packages=[]) ?target distro ocaml_version =
    Term.branch_head opam_repo "master" >>= fun opam_repo_commit ->
    Term_utils.term_map_s (fun (repo,branch) ->
      Term.branch_head repo branch >|= fun commit ->
      (repo,commit)
    ) extra_remotes >>= fun extra_remotes ->
    let remote_git_rev = Commit.hash opam_repo_commit in
    let pkg_target = String.concat ~sep:" " packages in
    let hum = Fmt.strf "base image for opam install %s" pkg_target in
    (match target with
    | None -> Term.return None
    | Some target -> Term.target target >|= fun t -> Some t) >>= fun target ->
    Opam_build.(run opam_t {packages;target;distro;ocaml_version;remote_git_rev;extra_remotes}) >>=
    Docker_build.run docker_t.DO.build_t ~hum >>= fun img ->
    Opam_ops.build_package docker_t img pkg_target

  let run_phases ?(extra_remotes=[]) () target =
    let packages = packages_of_repo target in
    let build = build ~extra_remotes ~packages ~target in
    (* phase 1 *)
    let ubuntu = build "ubuntu-16.04" primary_ocaml_version in
    let phase1 = ubuntu >>= fun _ -> Term.return () in
    (* phase 2 revdeps *)
    let pkg_revdeps =
      Term.without_logs ubuntu >>=
      Opam_ops.build_revdeps docker_t (packages_of_repo target) in
    let phase2 =
      Term_utils.after phase1 >>= fun () ->
      pkg_revdeps in
    (* phase 3 compiler variants *)
    let compiler_versions =
      List.map (fun oc ->
        let t = build "alpine-3.4" oc in
        ("OCaml "^oc), t
      ) compiler_variants in
    let phase3 =
      Term_utils.after phase2 >>= fun () ->
      Term.wait_for_all compiler_versions in
    (* phase 4 *)
    let debian = build "debian-stable" primary_ocaml_version in
    let ubuntu1604 = build "ubuntu-16.04" primary_ocaml_version in
    let centos7 = build "centos-7" primary_ocaml_version in
    let phase4 =
      Term_utils.after phase3 >>= fun () ->
      Term.wait_for_all [
        "Debian Stable", debian;
        "Ubuntu 16.04", ubuntu1604;
        "CentOS7", centos7 ] in
    (* phase 5 *)
    let debiant = build "debian-testing" primary_ocaml_version in
    let debianu = build "debian-unstable" primary_ocaml_version in
    let opensuse = build "opensuse-42.1" primary_ocaml_version in
    let fedora24 = build "fedora-24" primary_ocaml_version in
    let phase5 =
      Term_utils.after phase4 >>= fun () ->
      Term.wait_for_all [
        "Debian Testing", debiant;
        "Debian Unstable", debianu;
        "OpenSUSE 42.1", opensuse;
        "Fedora 24", fedora24 ]
    in
    let all_tests = 
      [ Term_utils.report ~order:1 ~label:"Build" phase1;
        Term_utils.report ~order:2 ~label:"Revdeps" phase2;
        Term_utils.report ~order:3 ~label:"Compilers" phase3;
        Term_utils.report ~order:4 ~label:"Common Distros" phase4;
        Term_utils.report ~order:5 ~label:"All Distros" phase5;
      ] in
    match Target.id target with
    |`Ref ["heads";"master"] -> all_tests
    |`PR _  -> all_tests
    | _ -> []

  let build_repo_diff target =
    let extra_remotes = [ mirage_dev_remote ] in
    let build_pr_diff =
       Opam_ops.packages_from_diff docker_t target >>= fun pkgs ->
       let builds = 
         List.map (fun pkg ->
           let t = build ~packages:[pkg] ~extra_remotes "ubuntu-16.04" "4.03.0" in
           pkg, t
         ) pkgs in
       Term.wait_for_all builds
    in
    match Target.id target with
    | `PR pr -> [Term_utils.report ~order:1 ~label:"Build" build_pr_diff]
    | _ -> []

  let tests = [
    Config.project ~id:"mirage/mirage" (run_phases ~extra_remotes:[mirage_dev_remote] ());
    Config.project ~id:"mirage/mirage-dev" build_repo_diff;
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

