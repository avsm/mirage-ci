(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring
open Datakit_ci
open Datakit_github
open Term.Infix
module OD = Opam_docker

open Docker_ops
module Cmds = struct
  open !Dockerfile

  let build_archive ?volume {build_t;run_t;_} rev =
    let dfile =
      from ~tag:"alpine" "ocurrent/opam" @@
      workdir "/home/opam/opam-repository" @@
      OD.Cmds.add_archive_script @@
      OD.Cmds.set_opam_repo_rev rev @@
      OD.Cmds.add_cache_dir
    in
    let hum = Fmt.strf "base image for opam2 archive (%s)" (String.with_range ~len:6 rev) in
    let volumes =
      match volume with
      | None -> []
      | Some h -> [h,(Fpath.v "/home/opam/opam-repository/cache")]
    in
    Docker_build.run build_t ~pull:true ~hum dfile >>= fun img ->
    let cmd = ["opam";"admin";"cache";"/home/opam/opam-repository/cache"] in
    Docker_run.run ~volumes ~tag:img.Docker_build.sha256 ~cmd run_t >>= fun log ->
    String.cuts ~sep:"\n" log |>
    List.filter (String.is_prefix ~affix:"[ERROR] Got some errors while") |> function
    | [] -> Term.return (img, "Archives rebuilt")
    | hd::_ -> Term.return (img, hd)

  let run_package ?volume ?(with_tests=false) t image pkg =
    let cmd = ["opam";"exec";"--";"opam-ci-install";pkg] in
    let hum = Fmt.strf "opam install %s%s" (if with_tests then "-t " else "") pkg in
    let volumes =
      match volume with
      | None -> []
      | Some h -> [h,(Fpath.v "/home/opam/opam-repository/cache")]
    in
    let env = if with_tests then ["OPAMWITHTEST","true"] else [] in
    let r = Docker_run.run ~volumes ~env ~tag:image.Docker_build.sha256 ~hum ~cmd t in
    let b = Docker_run.branch ~volumes ~env ~tag:image.Docker_build.sha256 ~cmd () in
    r, b

  let run_packages ?volume ?with_tests t image pkgs =
    List.map (fun pkg ->
      let t, branch = run_package ?volume ?with_tests t image pkg in
      pkg, t, branch
    ) pkgs |> fun r ->
    Term.wait_for_all (List.map (fun (a,b,_) -> (a,b)) r) >>= fun () ->
    Term.return r

  let list_all_packages {run_t;_} image =
    let cmd = ["opam";"list";"-a";"--columns=package";"--color=never";"--installable"] in
    Docker_run.run ~tag:image.Docker_build.sha256 ~cmd run_t >|=
    String.cuts ~empty:false ~sep:"\n" >|=
    List.filter (fun s -> not (String.is_prefix ~affix:"#" s)) >|=
    List.map (fun s -> String.trim s)

  let list_revdeps {run_t;_} image pkg =
    let cmd = ["opam";"list";"-s";"--color=never";"--depends-on";pkg;"--installable";"--all-versions";"--depopts";"--with-test";"--with-doc"] in
    Docker_run.run ~tag:image.Docker_build.sha256 ~cmd run_t >|=
    String.cuts ~empty:false ~sep:"\n" >|=
    List.map (fun s -> String.trim s)

  let run_revdeps ?volume ({run_t;_} as t) pkg image =
    let l = Fmt.strf "revdeps:%s" pkg in
    let terms =
      list_revdeps t image pkg >>=
      run_packages ?volume ~with_tests:true run_t image
    in
    Term.wait_for_all [(l, terms)]

end

let build_package {build_t;_} image pkg =
  let base_pkg, version_pkg = match String.cut ~sep:"." pkg with
    | None -> failwith ("Couldn't extract the version number from "^pkg)
    | Some x -> x
  in
  let open !Dockerfile in
  let dfile =
    from image.Docker_build.sha256 @@
    run "opam pin add -k version -yn %s %s" base_pkg version_pkg @@
    env ["OPAMSOLVERTIMEOUT","500"] @@
    run "opam exec -- opam-ci-install %s" pkg in
  let hum = Fmt.strf "opam install %s" pkg in
  Docker_build.run build_t ~hum dfile

let build_packages ~with_tests t image pkgs =
  let builds = List.map (fun pkg ->
    let image = build_package t image pkg in
    let image =
      if with_tests then
        image >>= fun image ->
        fst (Cmds.run_package ~with_tests:true t.run_t image pkg) >|= fun _ ->
        image
      else
        image
    in
    pkg, image
  ) pkgs in
  Term.catch (Term.wait_for_all builds) >|= fun _ -> (* Wait for all but do not fail yet *)
  let rec gather acc = function
    | [] -> Term.return (List.rev acc)
    | (l,hd)::tl -> hd >>= fun hd -> gather ((l,hd)::acc) tl in
  (builds, gather [] builds)

let packages_from_diff ?(default=["ocamlfind"]) {build_t;run_t;_} target =
  let opam_slug = Fmt.strf "%a" Repo.pp (Target.repo target) in
  match Target.id target with
  |`Ref _ -> Term.return default
  |`PR pr_num ->
    Term.target target >>= fun target ->
    let commit_str = Commit.hash (Target.head target) in
    let dfile =
      let open Dockerfile in
      from "alpine" @@
      run "apk --no-cache add curl" @@
      comment "" @@ (* NOTE: Avoids concat and thus to download curl everytime *)
      run "echo '#!/bin/sh -eu' >> /root/opam-github-pr-diff" @@
      run "echo 'REPO_SLUG=$1' >> /root/opam-github-pr-diff" @@
      run "echo 'PRNUM=$2' >> /root/opam-github-pr-diff" @@
      run "PR_COMMIT=%s" commit_str @@ (* NOTE: Force update for each new commit *)
      run {|echo 'curl -sL https://github.com/$REPO_SLUG/pull/$PRNUM.diff | \
                  sed -E -n -e '\''s,\+\+\+ b/packages/[^/]*/([^/]*)/.*,\1,p'\'' | \
                  sort -u' >> /root/opam-github-pr-diff|} @@
      run "chmod +x /root/opam-github-pr-diff" @@
      entrypoint_exec ["/root/opam-github-pr-diff"]
    in
    Docker_build.run build_t ~pull:true ~hum:"opam-diff image" dfile >>= fun img ->
    let cmd = [opam_slug; string_of_int pr_num] in
    Docker_run.run ~tag:img.Docker_build.sha256 ~cmd run_t >|=
    fun x -> String.cuts ~empty:false ~sep:"\n" x |> List.map String.trim

let distro_build ~packages ~target ~distro ~ocaml_version ~remotes ~typ ~opam_repo ~with_tests opam_t docker_t selector =
  (* Get the commits for the mainline opam repo *)
  let opam_repo, opam_repo_branch = opam_repo in
  Term.branch_head opam_repo opam_repo_branch >>= fun opam_repo_commit ->
  let opam_repo_remote = {Opam_docker.Remote.repo=opam_repo; commit=opam_repo_commit; full_remote=true} in
  (* Get commits for any extra OPAM remotes *)
  Term_utils.term_map_s (fun (repo,branch) ->
    Term.branch_head repo branch >|= fun commit ->
    {Opam_docker.Remote.repo; commit; full_remote=false}
  ) remotes >>= fun remotes ->
  let remotes = opam_repo_remote :: remotes in
  Term.target target >>= fun target ->
  Opam_build.run ~packages ~target ~distro ~ocaml_version ~remotes ~typ opam_t >>= fun df ->
  let hum = Fmt.(strf "base image for opam install %a" (list ~sep:sp string) packages) in
  Docker_build.run docker_t.Docker_ops.build_t ~pull:true ~hum df >>= fun img ->
  build_packages ~with_tests docker_t img packages >>= selector

let run_phases ?volume ?(build_filter=Term.return true) ~revdeps ~packages ~remotes ~typ ~opam_repo opam_t docker_t target =
  let build_no_return ~with_tests distro ocaml_version selector =
    build_filter >>= function
    | true ->
        packages >>= begin fun packages -> match packages with
        | _::_ -> distro_build ~packages ~target ~distro ~ocaml_version ~remotes ~typ ~opam_repo ~with_tests opam_t docker_t selector
        | [] -> selector ([], Term.return [])
        end
    | false ->
        selector ([], Term.return [])
  in
  let build ~with_tests distro ocaml_version = build_no_return ~with_tests distro ocaml_version snd in
  (* phase 1 *)
  let primary_version = Oversions.primary in
  let debian_stable = build_no_return ~with_tests:false "debian-10" primary_version Term.return in
  let phase1 = debian_stable >>= fun (_, t) -> t >>= fun _ -> Term.return () in
  (* phase 2 revdeps *)
  let phase2 =
    debian_stable >>= fun (debian_stable, _) ->
    let ts = List.map (fun (l,img) ->
      let t = img >>= Cmds.run_revdeps ?volume docker_t l in
      (Fmt.strf "revdep:%s" l), t
    ) debian_stable in
    Term.wait_for_all ts in
  (* phase 3 compiler variants *)
  let compiler_versions =
      List.map (fun oc ->
        let t = build ~with_tests:true "debian-10" oc in
        ("OCaml "^Oversions.to_string oc), t
      ) Oversions.recents in
    let phase3 = Term.wait_for_all compiler_versions in
    (* phase 4 *)
    let alpine = build ~with_tests:true "alpine-3.12" primary_version in
    let ubuntu_lts = build ~with_tests:false "ubuntu-18.04" primary_version in
    let ubuntu_2004 = build ~with_tests:false "ubuntu-20.04" primary_version in
    let centos = build ~with_tests:false "centos-8" primary_version in
    let phase4 =
      Term.wait_for_all
        [ "Ubuntu LTS", ubuntu_lts;
          "Alpine", alpine;
          "Ubuntu 20.04", ubuntu_2004;
          "CentOS", centos ] in
    (* phase 5 *)
    let debiant = build ~with_tests:false "debian-testing" primary_version in
    let debianu = build ~with_tests:false "debian-unstable" primary_version in
    let opensuse = build ~with_tests:false "opensuse-15.2" primary_version in
    let fedora = build ~with_tests:false "fedora-32" primary_version in
    let phase5 =
      Term.wait_for_all
        [ "Debian Testing", debiant;
          "Debian Unstable", debianu;
          "Fedora", fedora;
          "OpenSUSE", opensuse ]
    in
    [   Term_utils.report ~order:1 ~label:"Build" phase1;
        Term_utils.report ~order:3 ~label:"Compilers" phase3;
        Term_utils.report ~order:4 ~label:"Common Distros" phase4;
        Term_utils.report ~order:5 ~label:"All Distros" phase5;
    ] @ (match revdeps with false -> [] | true ->
        [Term_utils.report ~order:2 ~label:"Revdeps" phase2])

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
