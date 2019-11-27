(*---------------------------------------------------------------------------
   Copyright (c) 2016 Anil Madhavapeddy. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open !Astring
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

(* If remote is not ocaml/opam-repository, we need to fetch its refs *)
let set_origin =
  let open Dockerfile in
  function
  | Some {Remote.repo;_} when (Datakit_github.User.name repo.Repo.user <> "ocaml") || repo.Repo.repo <> "opam-repository" ->
     run "git remote set-url origin git://github.com/%s/%s" (Datakit_github.User.name repo.Repo.user) repo.Repo.repo
  | _ -> empty

let generate_sh targ lines =
  let open Dockerfile in
  "#!/bin/sh" :: "" :: lines |>
  String.concat ~sep:"\\n" |> fun lines ->
  run "printf '%s' > %s && chmod a+x %s && sudo mv %s /usr/bin/%s" lines targ targ targ targ

module Cmds = struct
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
    run "opam pin add -y opam-ci-scripts https://github.com/kit-ty-kate/opam-ci-scripts.git"

  let base ~ocaml_version ~distro =
    from ~tag:distro "ocaml/opam2" @@
    add_cache_dir @@
    run "opam switch %s" (Oversions.to_string ocaml_version) @@
    run "git pull origin master" @@
    run "opam update" @@
    run "opam install -yv opam-depext%s"
      (if Oversions.older_than_4_06 ocaml_version
       then " ocaml-secondary-compiler" (* NOTE: This is needed since dune 2.0.0
                                           requires at least OCaml 4.06 OR this package
                                           to build, which takes more than 4 minutes
                                           to compile *)
       else "")

  let add_archive_script =
    generate_sh "opam-ci-archive" [
      "sudo chown opam /home/opam/opam-repository/cache";
      "opam admin cache"
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
