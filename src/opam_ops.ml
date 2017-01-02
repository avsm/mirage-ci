(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring
open Datakit_ci
open Datakit_github
open Term.Infix
open Docker_ops
module OD = Opam_docker

module V1 = struct
  open !Dockerfile

  let build_archive ?volume docker_build_t docker_run_t rev =
    let dfile =
      let open Dockerfile in
      from ~tag:"alpine_ocaml-4.03.0" "ocaml/opam" @@
      OD.V1.set_opam_repo_rev rev in
    let hum = Fmt.strf "base image for opam archive (%s)" (String.with_range ~len:6 rev) in
    let volumes =
      match volume with
      | None -> []
      | Some h -> [h,(Fpath.v "/home/opam/opam-repository/archives")]
    in
    Docker_build.run docker_build_t ~hum dfile >>= fun img ->
    let cmd = ["sh";"-c";"sudo chown opam /home/opam/opam-repository/archives && opam admin make"] in
    Docker_run.run ~volumes ~tag:img.Docker_build.sha256 ~cmd docker_run_t 

  let list_all_packages t image =
    let cmd = ["opam";"list";"-a";"-s";"--color=never"] in
    Docker_run.run ~tag:image.Docker_build.sha256 ~cmd t >|=
    String.cuts ~empty:false ~sep:"\n" >|=
    List.map (fun s -> String.trim s)

end

module V2 = struct
  open !Dockerfile

  let build_archive ?volume docker_build_t docker_run_t rev =
    let dfile =
      from ~tag:"alpine_ocaml-4.03.0" "ocaml/opam-dev" @@
      OD.V2.set_opam_repo_rev rev @@
      run "echo 'archive-mirrors: [ \"file:///home/opam/opam-repository/cache\" ]' >> /home/opam/.opam/config"
    in
    let hum = Fmt.strf "base image for opam2 archive (%s)" (String.with_range ~len:6 rev) in
    let volumes =
      match volume with
      | None -> []
      | Some h -> [h,(Fpath.v "/home/opam/opam-repository/cache")]
    in
    Docker_build.run docker_build_t ~hum dfile >>= fun img ->
    let cmd = ["sh";"-c";"sudo chown opam /home/opam/opam-repository/cache && opam admin make"] in
    Docker_run.run ~volumes ~tag:img.Docker_build.sha256 ~cmd docker_run_t 

  let run_package ?volume t image pkg =
    let cmd = ["opam";"depext";"-ivyj";"2";pkg] in
    let volumes =
      match volume with
      | None -> []
      | Some h -> [h,(Fpath.v "/home/opam/opam-repository/cache")]
    in
    let r = Docker_run.run ~volumes ~tag:image.Docker_build.sha256 ~cmd t in
    let b = Docker_run.branch ~volumes ~tag:image.Docker_build.sha256 ~cmd () in
    r, b

  let run_packages ?volume t image pkgs =
    List.map (fun pkg ->
      let t, branch = run_package ?volume t image pkg in
      pkg, t, branch
    ) pkgs |> fun r ->
    Term.wait_for_all (List.map (fun (a,b,_) -> (a,b)) r) |>
    Term_utils.ignore_failure ~on_fail:(fun _ -> ()) >>= fun () ->
    Term.return r

  let list_all_packages t image =
    let cmd = ["opam";"list";"-a";"-s";"--color=never";"--installable"] in
    Docker_run.run ~tag:image.Docker_build.sha256 ~cmd t >|=
    String.cuts ~empty:false ~sep:"\n" >|=
    List.map (fun s -> String.trim s)
end

let dfile_v1 ?(pins=[]) ?(remotes=[]) ~ocaml_version ~distro ~opam_repo_git_rev
  ~user ~repo ~branch ~commit () =
  let (@@) = Dockerfile.(@@) in
  OD.V1.base ~ocaml_version ~distro @@
  OD.V1.set_opam_repo_rev opam_repo_git_rev @@
  OD.V1.add_remotes remotes @@
  OD.V1.add_pins pins @@
  OD.V1.clone_src ~user ~repo ~branch ~commit ()

let dfile_package ?(pins=[]) ?(remotes=[]) ~target ~ocaml_version ~distro ~opam_repo_git_rev =
  let {Repo.user; repo} = Target.repo target in
  let branch =
    match Target.id target with
    | `PR pr -> Printf.sprintf "pull/%d/head" pr
    | `Ref r -> Fmt.strf "%a" Ref.pp_name r
  in
  Term.target target >>= fun target ->
  let commit = Commit.hash (Target.head target) in
  let dfile = dfile_v1 ~pins ~remotes ~ocaml_version ~distro ~opam_repo_git_rev ~user ~repo ~branch ~commit () in
  Term.return dfile

let build_package {build_t;_} image pkg =
  let open !Dockerfile in
  let dfile =
    from image.Docker_build.sha256 @@
    run "opam depext -y %s" pkg @@
    run "opam install -j 2 -yv %s" pkg in
  let hum = Fmt.strf "opam install %s" pkg in
  Docker_build.run build_t ~hum dfile

let build_packages t image pkgs =
  List.map (fun pkg ->
    let t = build_package t image pkg in
    pkg, t
  ) pkgs |>
  Term.wait_for_all |>
  Term_utils.ignore_failure ~on_fail:(fun _ -> ())

let list_revdeps {run_t} image pkg =
  let cmd = ["opam";"list";"-s";"--depends-on";pkg] in
  Docker_run.run ~tag:image.Docker_build.sha256 ~cmd run_t >|=
  String.cuts ~empty:false ~sep:"\n"

let build_revdeps ({build_t;_} as t) packages image =
  List.map (fun pkg ->
    let terms =
      list_revdeps t image pkg >>=
      build_packages t image in
    let l = Fmt.strf "revdeps:%s" pkg in
    l, terms) packages |>
  Term.wait_for_all

let packages_from_diff {pull_t;run_t;_} target =
  let opam_slug = Fmt.strf "%a" Repo.pp (Target.repo target) in
  match Target.id target with
  |`Ref _ -> Term.fail "Skipping, can only build PRs"
  |`PR pr_num ->
    let time = Ptime_clock.now () in
(*    Docker_pull.run ~slug:"unikernel/mirage-ci" ~tag:"opam-diff" ~time pull_t >>= fun img -> *)
    let cmd = [opam_slug; string_of_int pr_num] in
    Docker_run.run ~tag:"unikernel/mirage-ci:opam-diff" ~cmd run_t >|=
    fun x -> String.cuts ~empty:false ~sep:"\n" x |> List.map String.trim

let distro_build ?(extra_remotes=[]) ?(packages=[]) ?target ~opam_repo ~distro ~ocaml_version ~typ ~opam_t ~docker_t () =
  Term.branch_head (fst opam_repo) (snd opam_repo) >>= fun opam_repo_commit ->
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
  Opam_build.(run opam_t {packages;target;distro;ocaml_version;remote_git_rev;extra_remotes;typ}) >>=
  Docker_build.run docker_t.Docker_ops.build_t ~hum >>= fun img ->
  build_package docker_t img pkg_target

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

