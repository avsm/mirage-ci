(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring

open DataKitCI

let logs = Main.logs

module Builder = struct

  open Term.Infix

  let one_hour = 60. *. 60.
  let three_hours = one_hour *. 3.
  let opam_repo = ProjectID.v ~user:"mirage" ~project:"opam-repository"
  let mirage_dev_repo = ProjectID.v ~user:"mirage" ~project:"mirage-dev"
  let mirage_dev_branch = "master"
  let mirage_dev_remote = mirage_dev_repo, mirage_dev_branch
  let primary_ocaml_version = "4.03.0"
  let compiler_variants = [("4.02.3",false);("4.03.0_flambda",false);("4.04.0",true)]

  let pool = Monitored_pool.create "docker" 5

  (* XXX TODO temporary until we can query package list automatically *)
  let packages_of_repo (project, _target) =
    match Fmt.strf "%a" ProjectID.pp project with
    | "mirage/ocaml-cohttp" -> ["cohttp"]
    | "mirage/mirage" -> ["mirage";"mirage-types"]
    | _ -> failwith "TODO package_of_repo"

  let label = "mir" 
  let docker_t = Docker_build.config ~logs ~label ~pool ~timeout:one_hour
  let docker_run_t = Docker_run.config ~logs ~label ~pool ~timeout:one_hour
  let opam_t = Opam_build.config ~logs ~label

  let rec term_map_s fn l =
    match l with
    | [] -> Term.return []
    | x :: l ->
        fn x >>= fun x ->
        term_map_s fn l >|= fun l ->
        x :: l

  (* revdep handling *) 
  let build_revdep image pkg =
    let open !Dockerfile in
    let dfile =
      from image.Docker_build.sha256 @@
      run "opam depext -uiyv -j 2 %s" pkg
    in
    let hum = Fmt.strf "opam install %s" pkg in
    Docker_build.run docker_t ~hum dfile

  let build_revdeps image pkgs =
    String.cuts ~empty:false ~sep:"\n" pkgs |> fun pkgs ->
    Term.list_map_p (fun pkg ->
      build_revdep image pkg |>
      Term.state >>= function
      | Error (`Pending _) -> Term.return (`Pending pkg)
      | Error (`Failure _) -> Term.return (`Failure pkg)
      | Ok _               -> Term.return (`Ok pkg)
    ) pkgs >>= fun results ->
    List.fold_left (fun (succ,fail,pending) r ->
      match r with
      | `Pending pkg -> succ, fail, (pkg::pending)
      | `Failure pkg -> succ, (pkg::fail), pending
      | `Ok pkg      -> (pkg::succ), fail, pending
    ) ([],[],[]) results |> fun (succ,fail,pending) ->
    let status label rs =
      match rs with
      | [] -> ""
      | rs -> Fmt.strf "%s:%d " label (List.length rs)
    in
    let status = Fmt.strf "%s%s%s" (status "Ok" succ) (status "Err" fail) (status "Building" pending) in
    match pending with
    | [] -> Term.return status
    | _ -> Term.pending "%s" status

  let calculate_revdeps image pkg =
    let cmd = ["opam";"list";"-s";"--depends-on";pkg] in
    Docker_run.run ~tag:image.Docker_build.sha256 ~cmd docker_run_t

  let revdeps target image =
    packages_of_repo target |>
    List.map (fun pkg ->
      let t = calculate_revdeps image pkg >>= build_revdeps image in
      let l = Fmt.strf "revdeps:%s" pkg in
      l, t) |>
    Term.wait_for_all

  (* base building *)
  let build ?(extra_remotes=[]) target distro ocaml_version = 
    let packages = packages_of_repo target in
    Term.branch_head opam_repo "master" >>= fun opam_repo_commit ->
    term_map_s (fun (repo,branch) ->
      Term.branch_head repo branch >>= fun commit ->
      Term.return (repo,branch,commit)
    ) extra_remotes >>= fun extra_remotes ->
    let remote_git_rev = Github_hooks.Commit.hash opam_repo_commit in
    Term.github_target target >>= fun target ->
    let hum = Fmt.(strf "opam install %a" (list ~sep:Format.pp_print_space string) packages) in
    Opam_build.(run opam_t {packages;target;distro;ocaml_version;remote_git_rev;extra_remotes}) >>=
    Docker_build.run docker_t ~hum

  let report ?(allow_fail=false) label img =
    let term =
      Term.state img >>= function
      | Error (`Pending p) -> Term.pending "%s" p
      | Error (`Failure f) when allow_fail -> Term.return (Fmt.strf "(Allowed failure) %s" f)
      | Error (`Failure f) -> Term.fail "%s" f
      | Ok _img            -> Term.return label
    in
    label, term

  let after t =
    Term.wait_for ~while_pending:"waiting" ~on_failure:"depfail" t |>
    Term.without_logs

  let run_phases ?(extra_remotes=[]) () target =
    let build = build ~extra_remotes target in
    (* phase 1 *)
    let alpine = build "alpine-3.4" primary_ocaml_version in
    let phase1 = alpine in
    (* phase 2 revdeps *)
    let alpine_revdeps =
      Term.without_logs phase1 >>=
      revdeps target in
    let phase2 =
      after phase1 >>= fun () ->
      alpine_revdeps in
    (* phase 3 compiler variants *)
    let compiler_versions =
      List.map (fun (oc,allow_fail) ->
        build "alpine-3.4" oc |>
        report ~allow_fail ("OCaml "^oc)
      ) compiler_variants in
    let phase3 =
      after phase2 >>= fun () ->
      Term.wait_for_all compiler_versions in
    (* phase 4 *)
    let debian = build "debian-stable" primary_ocaml_version in
    let ubuntu1604 = build "ubuntu-16.04" primary_ocaml_version in
    let centos7 = build "centos-7" primary_ocaml_version in
    let phase4 =
      after phase3 >>= fun () ->
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
      after phase4 >>= fun () ->
      Term.wait_for_all [
        "Debian Testing", debiant;
        "Debian Unstable", debianu;
        "OpenSUSE 42.1", opensuse;
        "Fedora 24", fedora24 ]
    in
    let all_tests = 
      [ report "1 Build" phase1;
        report "2 Revdeps" phase2;
        report "3 Compilers" phase3;
        report "4 Common Distros" phase4;
        report "5 All Distros" phase5;
      ] in
    match DataKitCI.Target.Full.id target with
    |`Ref r when Datakit_path.to_hum r = "heads/master" -> all_tests
    |`PR _  -> all_tests
    | _ -> []

  let tests = [
    Config.project ~id:"mirage/mirage" (run_phases ~extra_remotes:[mirage_dev_remote] ());
  ]
end

(* Command-line parsing *)

let web_config =
  Web.config
    ~name:"mirage-ci"
    ~can_read:ACL.(everyone)
    ~can_build:ACL.(username "admin")
    ~state_repo:(Uri.of_string "https://github.com/mirage/mirage-ci.logs")
    ()

let () =
  Main.run (Cmdliner.Term.pure (Config.ci ~web_config ~projects:Builder.tests))

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

