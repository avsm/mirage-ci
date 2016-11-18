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

  let pool = Monitored_pool.create "docker" 35

  let project_of_target = function
    | `PR pr -> Github_hooks.PR.project pr
    | `Ref rf -> Github_hooks.Ref.project rf

  (* XXX TODO temporary until we can query package list automatically *)
  let package_of_repo target =
    let project = project_of_target target in
    match Fmt.strf "%a" ProjectID.pp project with
    | "mirage/ocaml-cohttp" -> "cohttp"
    | "mirage/mirage" -> "mirage"
    | _ -> failwith "TODO package_of_repo"

  let label = "mirageci2" 
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
    Docker_build.run docker_t dfile

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

  let revdeps image =
    Term.target >>= fun target ->
    package_of_repo target |>
    calculate_revdeps image >>=
    build_revdeps image

  (* base building *)
  let build ?(extra_remotes=[]) distro ocaml_version = 
    Term.target >>= fun target ->
    let package = package_of_repo target in
    Term.branch_head opam_repo "master" >>= fun opam_repo_commit ->
    term_map_s (fun (repo,branch) ->
      Term.branch_head repo branch >>= fun commit ->
      Term.return (repo,branch,commit)
    ) extra_remotes >>= fun extra_remotes ->
    let remote_git_rev = Github_hooks.Commit.hash opam_repo_commit in
    Opam_build.(run opam_t {package;target;distro;ocaml_version;remote_git_rev;extra_remotes}) >>=
    Docker_build.run docker_t

  let report ?(allow_fail=false) label img =
    let term =
      Term.state img >>= function
      | Error (`Pending p) -> Term.pending "%s" p
      | Error (`Failure f) when allow_fail -> Term.return (Fmt.strf "(Allowed failure) %s" f)
      | Error (`Failure f) -> Term.fail "%s" f
      | Ok _img            -> Term.return label
    in
    label, term

  let (>>~=) dep next =
    Term.state dep >>= function
    | Error (`Pending _) -> Term.pending "Blocked"
    | Error (`Failure _) -> Term.fail "Skipped"
    | Ok r -> next r

  let (>>?=) dep next = dep >>~= fun _ -> next

  let run_phases ?(extra_remotes=[]) () =
    let build = build ~extra_remotes in
    (* phase 1 *)
    let alpine = build "alpine-3.4" primary_ocaml_version in
    (* phase 1 revdeps *)
    let alpine_revdeps = alpine >>~= revdeps in
    (* phase 1 compiler variants *)
    let versions =
      List.map (fun (oc,allow_fail) ->
        let t = alpine >>?= build "alpine-3.4" oc in
        report ~allow_fail ("OCaml "^oc) t
      ) compiler_variants in
    (* phase 2 *)
    let debian = alpine >>?= build "debian-stable" primary_ocaml_version in
    let ubuntu1604 = alpine >>?= build "ubuntu-16.04" primary_ocaml_version in
    let centos7 = alpine >>?= build "centos-7" primary_ocaml_version in
    let phase2 = debian >>= fun _ -> ubuntu1604 >>= fun _ -> centos7 in
    (* phase 3 *)
    let debiant = phase2 >>?= build "debian-testing" primary_ocaml_version in
    let opensuse = phase2 >>?= build "opensuse-42.1" primary_ocaml_version in
    let fedora24 = phase2 >>?= build "fedora-24" primary_ocaml_version in
    (* test set *)
    [ report "Alpine 3.4" alpine;
      report "Debian Stable" debian;
      report "CentOS 7" centos7;
      report "Ubuntu 16.04" ubuntu1604;
      report "Debian Testing" debiant;
      report "OpenSUSE 42.1" opensuse;
      report "Fedora 24" fedora24;
      ("Revdeps",alpine_revdeps);
    ] @ versions

  let tests = [
    Config.project ~id:"mirage/ocaml-cohttp" (run_phases ());
    Config.project ~id:"mirage/mirage"       (run_phases ~extra_remotes:[mirage_dev_remote] ());
  ]
end

(* Command-line parsing *)

let web_config =
  Web.config
    ~name:"mirage-ci"
    ~can_read:ACL.(everyone)
    ~can_build:ACL.(github_org "mirage")
    ?state_repo:None
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

