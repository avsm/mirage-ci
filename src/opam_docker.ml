(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring
open Datakit_ci
open Datakit_github

module Remote = struct
  type t = {
    repo: Repo.t;
    commit: Commit.t;
    full_remote: bool;
  }

  let pp_for_compare ppf {repo; commit = _; full_remote } =
    Fmt.pf ppf "repo=%a full_remote=%b"
      Repo.pp repo full_remote
end

let repo ~user ~repo ~branch =
  Repo.v ~user:(Datakit_github.User.v user) ~repo, branch

let ocaml_opam_repository = repo ~user:"ocaml" ~repo:"opam-repository" ~branch:"master"
let mirage_opam_repository = repo ~user:"mirage" ~repo:"opam-repository" ~branch:"master"
let js_opam_repository = repo ~user:"janestreet" ~repo:"opam-repository" ~branch:"master"

module type V = sig
  val add_cache_dir : Dockerfile.t
  val add_remotes : Remote.t list -> Dockerfile.t
  val set_opam_repo_rev : ?remote:Remote.t -> ?branch:string -> ?dst_branch:string -> string -> Dockerfile.t
  val base : ocaml_version:string -> distro:string -> Dockerfile.t
  val clone_src : user:string -> repo:string -> branch:string -> commit:string -> Dockerfile.t
  val merge_src : user:string -> repo:string -> branch:string -> commit:string -> Dockerfile.t
  val add_local_pins : string list -> Dockerfile.t
  val switch_local_remote : Dockerfile.t
  val add_local_remote : Dockerfile.t
  val add_ci_script : Dockerfile.t
  val add_archive_script : Dockerfile.t
end

(* If remote is not ocaml/opam-repository, we need to fetch its refs *)
let set_origin =
  let open Dockerfile in
  function
  | Some {Remote.repo;commit;_} when (Datakit_github.User.name repo.Repo.user <> "ocaml") || repo.Repo.repo <> "opam-repository" ->
     run "git remote set-url origin git://github.com/%s/%s" (Datakit_github.User.name repo.Repo.user) repo.Repo.repo
  | _ -> empty

let generate_sh targ lines =
  let open Dockerfile in
  "#!/bin/sh" :: "" :: lines |>
  String.concat ~sep:"\\n" |> fun lines ->
  run "printf '%s' > %s && chmod a+x %s && sudo mv %s /usr/bin/%s" lines targ targ targ targ

module V1 = struct
  open !Dockerfile

  let add_cache_dir = Dockerfile.empty

  let add_remotes rs =
    let remotes_ref = ref 0 in
    List.map (fun {Remote.repo; commit; _} ->
     incr remotes_ref;
     run "opam remote add e%d https://github.com/%s.git#%s"
       !remotes_ref (Fmt.strf "%a" Repo.pp repo) (Commit.hash commit)
    ) rs |> fun remotes ->
    empty @@@ remotes

  let base ~ocaml_version ~distro =
    from ~tag:(distro^"_ocaml-"^ocaml_version) "ocaml/opam"

  let set_opam_repo_rev ?remote ?(branch="master") ?(dst_branch="cibranch") rev =
    workdir "/home/opam/opam-repository" @@
    set_origin remote @@
    run "git fetch origin %s:%s" branch dst_branch @@
    run "git checkout %s" rev

  let clone_src ~user ~repo ~branch ~commit =
    run "git clone git://github.com/%s/%s /home/opam/src" user repo @@
    workdir "/home/opam/src" @@
    run "git fetch origin %s:cibranch" branch @@
    run "git checkout %s" commit

  let merge_src ~user ~repo ~branch ~commit =
    workdir "/home/opam/opam-repository" @@
    run "git remote add local git://github.com/%s/%s" user repo @@
    run "git fetch local %s" branch @@
    run "git merge %s" commit

  let add_local_pins packages =
    empty @@@
    List.map (run "opam pin add -n %s /home/opam/src") packages

  let switch_local_remote =
    run "opam remote set-url default /home/opam/src"

  let add_local_remote =
    run "opam remote add local /home/opam/src"

  (* TODO support multiple args *)
  let add_ci_script =
    run "opam pin add -y opam-ci-scripts https://github.com/jpdeplaix/opam-ci-scripts.git"

  let add_archive_script =
    generate_sh "opam-ci-archive" [
      "sudo chown opam /home/opam/opam-repository/archives";
      "opam admin make 2>&1 | tee /tmp/build.log";
      "errs=`grep \"=== ERROR\" /tmp/build.log | awk -F\" \" \"{print \\$3}\" | xargs echo -n`";
      "numerrs=`grep \"=== ERROR\" /tmp/build.log | awk -F\" \" \"{print \\$3}\" | wc -l`";
      "num=`grep \"Packages to build\" /tmp/build.log | awk -F\" \" \"{print \\$4}\"`";
      "echo $num total, $num failed: $errs"
    ]
end

module V2 = struct
  open !Dockerfile

  let add_cache_dir =
    run "echo 'archive-mirrors: [ \"file:///home/opam/opam-repository/cache\" ]' >> /home/opam/.opam/config"

  let add_remotes rs =
    let remotes_ref = ref 0 in
    List.map (fun {Remote.repo; commit; _} ->
     incr remotes_ref;
     let dir_name = Fmt.strf "/home/opam/remotes/%d" !remotes_ref in
     let repo_name = Fmt.strf "%a" Repo.pp repo in
     run "git clone https://github.com/%s.git %s" repo_name dir_name @@
     run "cd %s && git checkout %s" dir_name (Commit.hash commit) @@
     run "opam remote add e%d %s" !remotes_ref dir_name
    ) rs |> fun remotes ->
    empty @@@ remotes

  let clone_src = V1.clone_src
  let merge_src = V1.merge_src
  let add_local_pins = V1.add_local_pins
  let add_ci_script = V1.add_ci_script
  let switch_local_remote = V1.switch_local_remote
  let add_local_remote = V1.add_local_remote
  let set_opam_repo_rev = V1.set_opam_repo_rev

  let base ~ocaml_version ~distro =
    from ~tag:(distro^"-ocaml-" ^ ocaml_version) "ocaml/opam2" @@
    add_cache_dir @@
    run "opam install -yv opam-depext"

  let add_archive_script =
    generate_sh "opam-ci-archive" [
      "sudo chown opam /home/opam/opam-repository/cache";
      "opam admin make 2>&1 | tee /tmp/build.log"
    ]
end

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

