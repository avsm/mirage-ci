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

module type V = sig
  open Term
  val build_archive : ?volume:Fpath.t -> Docker_ops.t -> string -> (Docker_build.image * string) t
  val run_package : ?volume:Fpath.t -> Docker_run.t -> Docker_build.image -> string -> string t * string
  val run_packages : ?volume:Fpath.t -> Docker_run.t -> Docker_build.image -> string list -> (string * string Datakit_ci.Term.t * string) list t
  val list_all_packages : Docker_ops.t -> Docker_build.image -> string list t
  val list_revdeps : Docker_ops.t -> Docker_build.image -> string -> string list t
  val run_revdeps: ?volume:Fpath.t -> Docker_ops.t -> string -> Docker_build.image -> unit t
end

open Docker_ops
module V1 = struct
  open !Dockerfile

  let build_archive ?volume {build_t;run_t;_} rev =
    let dfile =
      let open Dockerfile in
      from ~tag:"alpine_ocaml-4.03.0" "ocaml/opam" @@
      OD.V1.add_archive_script @@
      OD.V1.set_opam_repo_rev rev in
    let hum = Fmt.strf "base image for opam archive (%s)" (String.with_range ~len:6 rev) in
    let volumes =
      match volume with
      | None -> []
      | Some h -> [h,(Fpath.v "/home/opam/opam-repository/archives")]
    in
    Docker_build.run build_t ~pull:true ~hum dfile >>= fun img ->
    let cmd = ["opam-ci-archive"] in
    Docker_run.run ~volumes ~tag:img.Docker_build.sha256 ~cmd run_t >>= fun build ->
    String.cuts ~sep:"\n" build |> List.rev |> function
    | hd::_ -> Term.return (img, hd)
    | [] -> Term.fail "No output from opam-admin make"

  let list_all_packages {run_t;_} image =
    let cmd = ["opam";"list";"-a";"-s";"--color=never"] in
    Docker_run.run ~tag:image.Docker_build.sha256 ~cmd run_t >|=
    String.cuts ~empty:false ~sep:"\n" >|=
    List.map (fun s -> String.trim s)

  let list_revdeps {run_t;_} image pkg =
    let cmd = ["opam";"list";"-s";"--depends-on";pkg] in
    Docker_run.run ~tag:image.Docker_build.sha256 ~cmd run_t >|=
    String.cuts ~empty:false ~sep:"\n"

  let run_package ?volume t image pkg =
    let cmd = ["opam";"config";"exec";"--";"opam-ci-install";pkg] in
    let hum = Fmt.strf "opam install %s" pkg in
    let volumes =
      match volume with
      | None -> []
      | Some h -> [h,(Fpath.v "/home/opam/opam-repository/archives")]
    in
    let r = Docker_run.run ~volumes ~tag:image.Docker_build.sha256 ~hum ~cmd t in
    let b = Docker_run.branch ~volumes ~tag:image.Docker_build.sha256 ~cmd () in
    r, b

  let run_packages ?volume t image pkgs =
    List.map (fun pkg ->
      let t, branch = run_package ?volume t image pkg in
      pkg, t, branch
    ) pkgs |> fun r ->
    Term.wait_for_all (List.map (fun (a,b,_) -> (a,b)) r) >>= fun () ->
    Term.return r

  let run_revdeps ?volume ({run_t;_} as t) pkg image =
    let l = Fmt.strf "revdeps:%s" pkg in
    let terms =
      list_revdeps t image pkg >>=
      run_packages ?volume run_t image
    in
    Term.wait_for_all [(l, terms)]

end

module V2 = struct
  open !Dockerfile

  let build_archive ?volume {build_t;run_t;_} rev =
    let dfile =
      from ~tag:"alpine_ocaml-4.03.0" "ocaml/opam-dev" @@
      OD.V2.add_archive_script @@
      OD.V2.set_opam_repo_rev rev @@
      OD.V2.add_cache_dir
    in
    let hum = Fmt.strf "base image for opam2 archive (%s)" (String.with_range ~len:6 rev) in
    let volumes =
      match volume with
      | None -> []
      | Some h -> [h,(Fpath.v "/home/opam/opam-repository/cache")]
    in
    Docker_build.run build_t ~pull:true ~hum dfile >>= fun img ->
    let cmd = ["opam-ci-archive"] in
    Docker_run.run ~volumes ~tag:img.Docker_build.sha256 ~cmd run_t >>= fun log ->
    String.cuts ~sep:"\n" log |>
    List.filter (String.is_prefix ~affix:"[ERROR] Got some errors while") |> function
    | [] -> Term.return (img, "Archives rebuilt")
    | hd::_ -> Term.return (img, hd)

  let run_package ?volume t image pkg =
    let cmd = ["opam";"config";"exec";"--";"opam-ci-install";pkg] in
    let hum = Fmt.strf "opam install %s" pkg in
    let volumes =
      match volume with
      | None -> []
      | Some h -> [h,(Fpath.v "/home/opam/opam-repository/cache")]
    in
    let r = Docker_run.run ~volumes ~tag:image.Docker_build.sha256 ~hum ~cmd t in
    let b = Docker_run.branch ~volumes ~tag:image.Docker_build.sha256 ~cmd () in
    r, b

  let run_packages ?volume t image pkgs =
    List.map (fun pkg ->
      let t, branch = run_package ?volume t image pkg in
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
    let cmd = ["opam";"list";"-s";"--color=never";"--depends-on";pkg;"--installable";"--all-versions";"--depopts"] in
    Docker_run.run ~tag:image.Docker_build.sha256 ~cmd run_t >|=
    String.cuts ~empty:false ~sep:"\n" >|=
    List.map (fun s -> String.trim s)

  let run_revdeps ?volume ({run_t;_} as t) pkg image =
    let l = Fmt.strf "revdeps:%s" pkg in
    let terms =
      list_revdeps t image pkg >>=
      run_packages ?volume run_t image
    in
    Term.wait_for_all [(l, terms)]

end

let build_package ~opam_version {build_t;_} image pkg =
  let base_pkg, version_pkg = match String.cut ~sep:"." pkg with
    | None -> failwith ("Couldn't extract the version number from "^pkg)
    | Some x -> x
  in
  let open !Dockerfile in
  let dfile =
    from image.Docker_build.sha256 @@
    begin match opam_version with
    | `V1 -> empty (* opam1 pin fails when the package doesn't have any archive
                      to download. (for instance with meta packages) *)
    | `V2 -> run "opam pin add -k version -yn %s %s" base_pkg version_pkg
    end @@
    run "opam config exec -- opam-ci-install %s" pkg in
  let hum = Fmt.strf "opam install %s" pkg in
  Docker_build.run build_t ~hum dfile

let build_packages ~opam_version t image pkgs =
  let builds = List.map (fun pkg ->
    let t = build_package ~opam_version t image pkg in
    pkg, t
  ) pkgs in
  Term.wait_for_all builds >>= fun () ->
  let rec gather acc = function
    | [] -> Term.return (List.rev acc)
    | (l,hd)::tl -> hd >>= fun hd -> gather ((l,hd)::acc) tl in
  gather [] builds

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

let run_revdeps ?volume ~opam_version docker_t pkg img =
  match opam_version with
  |`V1 -> V1.run_revdeps ?volume docker_t pkg img
  |`V2 -> V2.run_revdeps ?volume docker_t pkg img

let distro_build ~packages ~target ~distro ~ocaml_version ~remotes ~typ ~opam_version ~opam_repo opam_t docker_t =
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
  Opam_build.run ~packages ~target ~distro ~ocaml_version ~remotes ~typ ~opam_version opam_t >>= fun df ->
  let hum = Fmt.(strf "base image for opam install %a" (list ~sep:sp string) packages) in
  Docker_build.run docker_t.Docker_ops.build_t ~pull:true ~hum df >>= fun img ->
  build_packages ~opam_version docker_t img packages

(* NOTE: Function used to temporarily disable brocken opam1 images *)
let wait_for_all_opam ~opam_version v12 v2 =
  Term.wait_for_all (v12 @ match opam_version with `V1 -> [] | `V2 -> v2)

let run_phases ?volume ~revdeps ~packages ~remotes ~typ ~opam_version ~opam_repo opam_t docker_t target =
  let build distro ocaml_version =
    packages >>= function
    | [] -> Term.return []
    | packages -> distro_build ~packages ~target ~distro ~ocaml_version ~remotes ~typ ~opam_version ~opam_repo opam_t docker_t
  in
  (* phase 1 *)
  let debian_stable = build "debian-9" Oversions.primary in
  let phase1 = debian_stable >>= fun _ -> Term.return () in
  (* phase 2 revdeps *)
  let pkg_revdeps =
    debian_stable >>= fun debian_stable ->
    let ts = List.map (fun (l,img) ->
      let t =
        run_revdeps ?volume ~opam_version docker_t l img in
      (Fmt.strf "revdep:%s" l), t
    ) debian_stable in
    Term.wait_for_all ts in
  let phase2 =
      Term_utils.after phase1 >>= fun () ->
      pkg_revdeps in
    (* phase 3 compiler variants *)
  let compiler_versions =
      List.map (fun oc ->
        let t = build "debian-9" oc in
        ("OCaml "^Oversions.to_string oc), t
      ) Oversions.recents in
    let phase3 =
      Term_utils.after phase1 >>= fun () ->
      Term.wait_for_all compiler_versions in
    (* phase 4 *)
    let alpine = build "alpine" Oversions.primary in
    let ubuntu1604 = build "ubuntu-16.04" Oversions.primary in
    let ubuntu_lts = build "ubuntu-lts" Oversions.primary in
    let centos = build "centos" Oversions.primary in
    let phase4 =
      Term_utils.after phase3 >>= fun () ->
      wait_for_all_opam ~opam_version
        [ "Ubuntu 16.04", ubuntu1604 ]
        [ "Alpine", alpine;
          "Ubuntu LTS", ubuntu_lts;
          "CentOS", centos ] in
    (* phase 5 *)
    let debiant = build "debian-testing" Oversions.primary in
    let debianu = build "debian-unstable" Oversions.primary in
    let opensuse = build "opensuse" Oversions.primary in
    let fedora = build "fedora" Oversions.primary in
    let phase5 =
      Term_utils.after phase4 >>= fun () ->
      wait_for_all_opam ~opam_version
        [ "Debian Testing", debiant;
          "Debian Unstable", debianu ]
        [ "Fedora", fedora;
          "OpenSUSE", opensuse ]
    in
    let lf = Fmt.strf "%s %s" (match opam_version with |`V1 -> "V1.2" |`V2 -> "V2.0") in
    [   Term_utils.report ~order:1 ~label:(lf "Build") phase1;
        Term_utils.report ~order:3 ~label:(lf "Compilers") phase3;
        Term_utils.report ~order:4 ~label:(lf "Common Distros") phase4;
        Term_utils.report ~order:5 ~label:(lf "All Distros") phase5;
    ] @ (match revdeps with false -> [] | true ->
        [Term_utils.report ~order:2 ~label:(lf "Revdeps") phase2])

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
